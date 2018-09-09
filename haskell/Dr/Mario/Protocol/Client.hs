{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Dr.Mario.Protocol.Client
	( initializeConnection
	, currentGameState
	, control
	, GameState(..)
	, GameDelta(..)
	, PlayerState(..)
	, ModeState(..)
	, Response(..)
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Monad
import Data.Attoparsec.ByteString (IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.Builder (hPutBuilder)
import Data.Default
import Data.List
import Data.Monoid
import Data.Foldable
import Data.Map (Map)
import Data.Word
import System.IO

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

import Dr.Mario.Model
import qualified Dr.Mario.Protocol.Raw as R

data Diagnostic
	= PrintWarning !R.PrintWarning
	| ParseFailure !ByteString !ByteString [String] String
	-- ^ The message that failed to parse, the part of the message that had not
	-- yet been consumed when the failure happened, contexts for the error, and
	-- a description of the error.
	| ImpossibleIncompleteParse !ByteString
	-- ^ The parser for messages should consume exactly one line. This
	-- diagnostic appears if a parser consumed an entire line and still wanted
	-- more input. The 'ByteString' is the line it consumed.
	| ImpossiblePartialParse !ByteString !ByteString
	-- ^ The parser for messages should consume exactly one line. This
	-- diagnostic appears if a parser succeeded without consuming an entire
	-- line. The arguments are the entire line and the unconsumed part of the
	-- line.
	| ParseWarning !R.ParseWarning
	| IllegalMessage !ProtocolState !R.ServerMessage
	-- ^ The server sent an illegal message that made so little sense that
	-- there was no hope but to ignore it and hope for the best. (There are
	-- more customized diagnostics for situations where there is a more
	-- targeted recovery plan.) The diagnostic includes the client's belief
	-- about what state the protocol was in at the time and the message itself.
	| DuplicatePlayer !R.PlayerIdentifier !PlayerState
	-- ^ The server sent us two game states for the same player. This is the
	-- old one, which we are throwing away in favor of a new one. We don't
	-- count this as a unique player for the purposes of switching from the
	-- @game_setup@ to @cleanup@ state.
	| MissingYou !(Map R.PlayerIdentifier PlayerState)
	-- TODO: maybe don't throw away the 'R.PlayerIdentifier' we chose for @you@

	-- ^ We've seen the right number of players to switch from @game_setup@ to
	-- @cleanup@, but none of them were @you@. We've arbitrarily picked one to
	-- act as @you@ (and subsequently forgotten the 'R.PlayerIdentifier' the
	-- server actually used for that player... this isn't likely to end well).
	| ExtraControlState !Word32 !Pill
	-- ^ The server sent a @state ... control ...@ message instead of a @state
	-- ... cleanup@ message during the @game_setup@ or @game_setup_zero@
	-- phases. We're going to ignore the extra stuff.
	| UnknownID !R.ServerMessage
	-- ^ The server sent a @control@ or @queue@ reply, but either for an
	-- 'R.Identifier' we never made a request for or for an 'R.Identifier' it
	-- had already replied to. We ignored it.
	deriving (Eq, Ord, Read, Show)

writeMsgs :: Handle -> TChan Diagnostic -> TChan R.ClientMessage -> IO ()
writeMsgs h dbg msgs = do
	hSetBuffering h (BlockBuffering (Just R.messageSize))
	go
	where
	go = do
		builder <- atomically $ do
			(warnings, builder) <- R.pp <$> readTChan msgs
			mapM_ (writeTChan dbg . PrintWarning) warnings
			return builder
		hPutBuilder h builder
		hFlush h
		go

-- TODO: should we try to avoid exhausting memory here...?
readMsgs :: Handle -> TChan Diagnostic -> TChan R.ServerMessage -> IO ()
readMsgs h dbg msgs = do
	hSetBuffering h LineBuffering
	go
	where
	go = do
		bs <- BS.hGetLine h
		atomically $ case A.parse R.parse bs of
			Fail rest cxts err -> writeTChan dbg (ParseFailure bs rest cxts err)
			Partial f -> writeTChan dbg (ImpossibleIncompleteParse bs)
			Done rest (msg, warnings) -> do
				writeTChan msgs msg
				unless (rest == mempty) (writeTChan dbg (ImpossiblePartialParse bs rest))
				mapM_ (writeTChan dbg . ParseWarning) warnings
		go

stderrDiagnostics :: TChan Diagnostic -> IO ()
stderrDiagnostics dbg = forever (atomically (readTChan dbg) >>= hPrint stderr)

negotiateVersion :: TChan Diagnostic -> TChan R.ServerMessage -> TChan R.ClientMessage -> IO Bool
negotiateVersion dbg fromServer toServer = go False where
	go success = atomically (readTChan fromServer) >>= \msg -> case msg of
		R.ProposeVersion v -> go (success || v == R.protocolVersion)
		R.RequestVersion -> when success (atomically $ writeTChan toServer (R.Version R.protocolVersion)) >> return success
		_ -> return False

data Connection = Connection
	{ refGameState :: TMVar IGameState
	, chanToServer :: TChan R.ClientMessage
	}

-- TODO: write some documentation about the two ways of writing a client (push
-- v. pull)

-- | @initializeConnection s2c c2s f@ initializes a connection with a server.
-- It will receive 'R.ServerMessage's on @s2c@, send 'R.ClientMessage's on
-- @c2s@, and print diagnostic information to 'stderr'. Blocks until version
-- negotiation with the server is finished; returns 'Nothing' if version
-- negotiation fails.
--
-- You should not do anything with the handles after calling
-- 'initializeConnection'. If version negotiation fails, they will be closed
-- for you.
--
-- @f@ will be invoked (in its own thread) whenever something especially
-- interesting happens.
initializeConnection :: Handle -> Handle -> (GameDelta -> IO a) -> IO (Maybe Connection)
initializeConnection hFromServer hToServer deltaCallback = do
	chanDbg <- newTChanIO
	chanFromServer <- newTChanIO
	chanToServer <- newTChanIO
	threadToServer <- forkIO (writeMsgs hToServer chanDbg chanToServer)
	threadFromServer <- forkIO (readMsgs hFromServer chanDbg chanFromServer)
	threadDbg <- forkIO (stderrDiagnostics chanDbg)
	goodVersion <- negotiateVersion chanDbg chanFromServer chanToServer
	if goodVersion
	then do
		refGameState <- newTMVarIO (ISetup def)
		forkIO (handleMessages chanDbg refGameState chanFromServer deltaCallback)
		return (Just (Connection refGameState chanToServer))
	else do
		killThread threadToServer
		killThread threadFromServer
		-- threadDbg will get killed automatically once it's waiting on an
		-- empty TChan, and we don't really want or need to kill it earlier
		hClose hFromServer
		hClose hToServer
		return Nothing

currentGameState :: Connection -> IO GameState
currentGameState conn = atomically (readTMVar (refGameState conn)) >>= freezeGameState

-- | Start pressing buttons on a given frame number (and discard any previously
-- scheduled button presses on or after that frame). The callback supplied will
-- be called exactly once.
control :: Connection -> Word32 -> [R.ButtonPress] -> (Response () -> IO a) -> IO ()
control conn frame bps callback_ = do
	let callback = void . callback_
	igs <- atomically $ takeTMVar (refGameState conn)
	(igs', discarded) <- case igs of
		IInProgress cbControl n ips m -> do
			let (ident, cbControl') = fresh callback cbControl
			atomically $ writeTChan (chanToServer conn) (R.Control ident frame bps)
			return (IInProgress cbControl' n ips m, False)
		_ -> return (igs, True)
	atomically $ putTMVar (refGameState conn) igs'
	when discarded (void . forkIO $ callback Discarded)

-- | The protocol's @version_proposal@ and @version_selection@ states are not
-- represented here; the connection initialization methods don't return until
-- version negotiation is finished.
data GameState
	= Setup !(Map R.Identifier PlayerState)
	-- ^ The protocol's @game_setup_zero@ and @game_setup@ states are collapsed
	-- into 'Setup'. The 'Map' may have 'R.you' as a key.
	| InProgress !Word32 !PlayerState !(Map R.Identifier PlayerState)
	-- ^ The protocol's @cleanup@ and @control@ states are collapsed into
	-- 'InProgress'. You can check the first 'PlayerState' to see whether the
	-- official protocol state is @cleanup@ or @control@.
	--
	-- The fields are:
	--
	-- 1. Current frame number
	-- 2. This client's state
	-- 3. Opponents' state (if any); will not have 'R.you' as a key
	deriving (Eq, Ord, Read, Show)

data GameDelta
	= GameStarted !PlayerState !(Map R.Identifier PlayerState)
	| Winner !R.PlayerIdentifier
	deriving (Eq, Ord, Read, Show)

data PlayerState = PlayerState
	{ dropRate :: !Word32
	, pillLookahead :: !PillContent
	, board :: !Board
	, mode :: !ModeState
	, dead :: !Bool
	} deriving (Eq, Ord, Read, Show)

data ModeState
	= Cleanup
	| Control !Word32 !Pill -- ^ on which frame the pill will be next forced to drop one row, what the pill is
	deriving (Eq, Ord, Read, Show)

-- | A reification of the protocol's state machine, without any of the
-- additional information that should be available in each state.
data ProtocolState
	= VersionProposalState
	| VersionSelectionState
	| GameSetupZeroState
	| GameSetupState
	| CleanupState
	| ControlState
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The internal version of the 'GameState', that includes extra stuff we need
-- to track to get the protocol right, mutable versions of the board for
-- efficiency, and so forth.
--
-- Extra fields come first.
data IGameState
	= ISetup !(Map R.Identifier IPlayerState)
	| IInProgress !CallbackMap
	              !Word32 !IPlayerState !(Map R.Identifier IPlayerState)
	-- ^ Extra field: callbacks for @control@ messages

data Response a
	= Accept !a -- ^ The server accepted your request, and replied with the data included here
	| Far -- ^ The server rejected your request because it was for a time too far in the future
	| Old -- ^ The server rejected your request because it was about the past
	| Discarded -- ^ The server discarded your request because the conditions of the request didn't come up before the game ended
	deriving (Eq, Ord, Read, Show)

type CallbackMap = IdentifierMap (Response () -> IO ())

-- | A type for creating fresh 'R.Identifier's and associating them with
-- values.
data IdentifierMap a = IdentifierMap
	{ nextIdent :: !Word -- 2^30 identifiers ought to be enough for anybody
	, callbacks :: !(Map R.Identifier a)
	} deriving (Eq, Ord, Read, Show)

instance Default (IdentifierMap a) where def = IdentifierMap def def

-- | Associate a fresh identifier with a given value.
fresh :: a -> IdentifierMap a -> (R.Identifier, IdentifierMap a)
fresh a m = (ident, m
	{ nextIdent = nextIdent m + 1
	, callbacks = M.insert ident a (callbacks m)
	})
	where
	ident = identifierFromWord (nextIdent m)

discharge :: IdentifierMap a -> R.Identifier -> Maybe (a, IdentifierMap a)
discharge m k = case M.updateLookupWithKey (\_ _ -> Nothing) k (callbacks m) of
	(Nothing, _) -> Nothing
	(Just a, callbacks') -> Just (a, m { callbacks = callbacks' })

identifierFromWord :: Word -> R.Identifier
identifierFromWord = R.unsafeIdentifier . LBS.toStrict . Builder.toLazyByteString . go where
	-- we skip 121 ('y') so that we never construct the identifier "you" by accident
	[a, b, c] = sort [R.messageSeparator, R.componentSeparator, 121]
	go 0 = mempty
	go n = Builder.word8 firstByte <> go q where
		(q, r_) = n `quotRem` 253
		firstByte = case fromIntegral r_ of
			r | r < a     -> r
			  | r < b-1   -> r+1
			  | r < c-2   -> r+2
			  | otherwise -> r+3

data IPlayerState = IPlayerState
	{ iCachedFrozenState :: TVar (Maybe PlayerState)
	, iDropRate :: !Word32
	, iPillLookahead :: !PillContent
	, iBoard :: !IOBoard
	, iMode :: !ModeState
	, iDead :: !Bool
	}

-- | Pick out 'R.you', unless it ain't there, in which case pick out something
-- (anything, as long as it's a consistent choice), unless there's nothing
-- there, in which case 'Nothing' is there.
selectYou :: Map R.Identifier a -> Maybe ((R.Identifier, a), Map R.Identifier a)
selectYou m = case M.updateLookupWithKey (\_ _ -> Nothing) R.you m of
	(Just v, m') -> Just ((R.you, v), m')
	_ -> M.minViewWithKey m

freezePlayerState :: IPlayerState -> IO PlayerState
freezePlayerState ips = readTVarIO (iCachedFrozenState ips) >>= \case
	Just ps -> return ps
	Nothing -> do
		frozenBoard <- mfreeze (iBoard ips)
		let playerState = PlayerState
		    	{ dropRate = iDropRate ips
		    	, pillLookahead = iPillLookahead ips
		    	, board = frozenBoard
		    	, mode = iMode ips
		    	, dead = iDead ips
		    	}
		atomically $ writeTVar (iCachedFrozenState ips) (Just playerState)
		return playerState

thawPlayerState :: PlayerState -> IO IPlayerState
thawPlayerState ps = do
	cachedPlayerState <- newTVarIO (Just ps)
	iBoard <- thaw (board ps)
	return IPlayerState
		{ iCachedFrozenState = cachedPlayerState
		, iDropRate = dropRate ps
		, iPillLookahead = pillLookahead ps
		, iBoard = iBoard
		, iMode = mode ps
		, iDead = dead ps
		}

freezeGameState :: IGameState -> IO GameState
freezeGameState (ISetup m) = Setup <$> traverse freezePlayerState m
freezeGameState (IInProgress _ n ips m) = InProgress n <$> freezePlayerState ips <*> traverse freezePlayerState m

handleMessages :: TChan Diagnostic -> TMVar IGameState -> TChan R.ServerMessage -> (GameDelta -> IO a) -> IO ()
handleMessages dbg refGameState msgs deltaCallback = forever $ do
	msg <- atomically $ readTChan msgs
	-- We could use readTMVar/swapTMVar instead of takeTMVar/putTMVar
	-- here to reduce the time we hold the lock. However, for cases that
	-- read/alter mutable boards, we would need to be careful to hold the lock
	-- while we accessed the boards, which would make it much more difficult to
	-- separate concerns in the way handleMessages and handleMessage currently
	-- does (namely: all concurrency-awareness in handleMessages, all
	-- state-transition logic in handleMessage). Since holding the lock while
	-- handling all message types -- not just the few that muck with boards --
	-- is probably good enough in almost all cases, we do it the simple way.
	iGameState <- atomically $ takeTMVar refGameState
	(diagnostics, delta, iGameState') <- handleMessage iGameState msg
	atomically $ do
		mapM_ (writeTChan dbg) diagnostics
		putTMVar refGameState iGameState'
	traverse_ (forkIO . void . deltaCallback) delta

-- TODO: WriterT [Diagnostic] IO IGameState?
handleMessage :: IGameState -> R.ServerMessage -> IO ([Diagnostic], Maybe GameDelta, IGameState)
handleMessage (ISetup players) (R.State player dropFrames pill initialBoard mode) = do
	iPlayerState <- thawPlayerState $ PlayerState
		{ dropRate = dropFrames
		, pillLookahead = pill
		, board = initialBoard
		, mode = Cleanup
		, dead = False
		}

	let (oldVal_, players') = M.insertLookupWithKey (\_ _ _ -> iPlayerState) player iPlayerState players
	oldVal <- traverse freezePlayerState oldVal_
	let diagnostics =  foldMap (\v -> [DuplicatePlayer player v]) oldVal
	                ++ [ExtraControlState dropFrames' pill' | R.ControlState dropFrames' pill' <- [mode]]

	return (diagnostics, Nothing, ISetup players')

handleMessage (ISetup (selectYou -> Just ((you, ips), players))) (R.Frame n) = do
	frozenYou <- freezePlayerState ips
	frozenOpponents <- traverse freezePlayerState players
	return
		( [MissingYou (M.insert you frozenYou frozenOpponents) | you /= R.you]
		, Just (GameStarted frozenYou frozenOpponents)
		, IInProgress def n ips players
		)

handleMessage igs@(ISetup players) msg = return ([IllegalMessage protocolState msg], Nothing, igs) where
	protocolState = if M.size players == 0 then GameSetupZeroState else GameSetupState

handleMessage igs@(IInProgress cbControl n ips m) msg = case msg of
	R.AcceptControl id -> triggerControlCallback id (Accept ())
	R.FarControl    id -> triggerControlCallback id Far
	R.OldControl    id -> triggerControlCallback id Old

	where

	triggerControlCallback id resp = case discharge cbControl id of
		Just (callback, cbControl') -> do
			forkIO (callback resp)
			return ([], Nothing, IInProgress cbControl' n ips m)
		Nothing -> return ([UnknownID msg], Nothing, igs)

discardAll :: IdentifierMap (Response a -> IO ()) -> IO ()
discardAll m = traverse_ ($Discarded) (callbacks m)
