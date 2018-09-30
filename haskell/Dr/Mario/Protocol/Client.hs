{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Dr.Mario.Protocol.Client
	( -- * High-level interface
	  initializeConnection
	, currentGameState
	, queue
	, control
	, requestState
	, debug
	-- * Types
	, Connection
	, GameState(..)
	, GameDelta(..)
	, PlayerState(..)
	, ModeState(..)
	, Response(..)
	-- * Low-level connection initialization
	, FIFOs(..)
	, CreateFIFOsArgs(..)
	, createFIFOs
	, OpenFIFOsArgs(..)
	, openFIFOs
	, LaunchFCEUXArgs(..)
	, launchFCEUX
	, ConnectToHandlesArgs(..)
	, connectToHandles
	, InitializeConnectionArgs(..)
	, Diagnostic(..)
	-- * Convenient re-exports
	, module Dr.Mario.Model
	, R.StateRequestTime(..)
	, R.ButtonPress(..)
	, R.Button(..)
	, R.ButtonAction(..)
	, R.PlayerIdentifier, R.you
	, Word32
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Attoparsec.ByteString (IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.Builder (hPutBuilder)
import Data.Default
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Functor.Compose
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import System.IO
import System.IO.Error
import System.Process
import System.Timeout

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
	-- ^ We're switching from @game_setup@ to @cleanup@, but none of the
	-- players we've seen so far were @you@. We've arbitrarily picked one to
	-- act as @you@.
	| ExtraControlState !Word32 !Pill
	-- ^ The server sent a @state ... control ...@ message instead of a @state
	-- ... cleanup@ message during the @game_setup@ or @game_setup_zero@
	-- phases. We're going to ignore the extra stuff.
	| UnknownID !R.ServerMessage
	-- ^ The server sent a @control@ or @queue@ reply, but either for an
	-- 'R.Identifier' we never made a request for or for an 'R.Identifier' it
	-- had already replied to. We ignored it.
	| UnknownFrame !R.Word32 !(Response ())
	-- ^ The server rejected a @state-request@, but the frame it refused to
	-- report a state on doesn't match any of the frames we made requests
	-- about. We ignored the rejection.
	| UnknownPlayer !R.PlayerIdentifier
	-- ^ The server sent a message about a player that wasn't mentioned during
	-- game setup. We took it in stride the best we could.
	| IllegalPill !R.PlayerIdentifier !Pill
	-- ^ The server sent a player to cleanup mode while their pill was off the
	-- board or hovering over some existing stuff. We ignored the pill
	-- placement, but did go ahead and enter cleanup mode. Things are very
	-- likely to be out of sync now.
	| DoubleCleanup !R.PlayerIdentifier
	-- ^ The server sent a player to cleanup mode while they were already in
	-- cleanup mode. Weird.
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
readMsgs :: Handle -> TChan Diagnostic -> TChan (Maybe R.ServerMessage) -> IO ()
readMsgs h dbg msgs = do
	hSetBuffering h LineBuffering
	atomically (goBS mempty) >>= goPartial
	where
	goPartial (f, bs) = do
		bs1 <- BS.hGet h 1
		bs2 <- BS.hGetNonBlocking h 8191
		let bs' = bs1 <> bs2
		if BS.length bs1 + BS.length bs2 == 0
		then atomically (writeTChan msgs Nothing)
		else atomically (goResult (bs <> bs') (f bs')) >>= goPartial

	goResult bs (Fail rest cxts err) = do
		writeTChan dbg (ParseFailure bs rest cxts err)
		goBS . BS.drop 1 . BS.dropWhile (R.messageSeparator/=) $ rest
	goResult bs (Partial f) = return (f, bs)
	goResult bs (Done rest (msg, warnings)) = do
		writeTChan msgs (Just msg)
		mapM_ (writeTChan dbg . ParseWarning) warnings
		goBS rest

	goBS bs = goResult bs (A.parse R.parse bs)

negotiateVersion :: TChan Diagnostic -> TChan (Maybe R.ServerMessage) -> TChan R.ClientMessage -> IO Bool
negotiateVersion dbg fromServer toServer = go False where
	go success = atomically (readTChan fromServer) >>= \msg -> case msg of
		Just (R.ProposeVersion v) -> go (success || v == R.protocolVersion)
		Just R.RequestVersion -> when success (atomically $ writeTChan toServer (R.Version R.protocolVersion)) >> return success
		_ -> return False

-- | Information needed to send messages to a server.
data Connection = Connection
	{ refGameState :: TMVar IGameState
	, chanToServer :: TChan R.ClientMessage
	}

-- TODO: write some documentation about the two ways of writing a client (push
-- v. pull)

data InitializeConnectionArgs a = InitializeConnectionArgs
	{ icCreateFIFOs :: CreateFIFOsArgs
	, icOpenFIFOs :: OpenFIFOsArgs ()
	, icLaunch :: LaunchFCEUXArgs a ()
	, icConnect :: ConnectToHandlesArgs ()
	, icKillTimeout :: Int
	-- ^ If version negotiation fails, we ask the server to shut itself down.
	-- We give it this many microseconds after the request to comply, then the
	-- request becomes more forceful. Defaults to 1s.
	}

-- See [NOTE: (), not Default]
instance a ~ () => Default (InitializeConnectionArgs a) where
	def = InitializeConnectionArgs
		{ icCreateFIFOs = def
		, icOpenFIFOs = def
		, icLaunch = def
		, icConnect = def
		, icKillTimeout = 1000000
		}

-- | Initialize a connection with an FCEUX-based server. Blocks until version
-- negotiation with the server is finished; throws exceptions willy-nilly if
-- everything isn't to its liking.
--
-- If you find yourself trying to catch exceptions thrown by
-- 'initializeConnection', let's you and me chat about making a type that
-- embodies the different kinds of errors and making this return a suitable
-- `Either`. Otherwise you're going to be really unhappy with the hacks you
-- have to do.
initializeConnection :: InitializeConnectionArgs FilePath -> IO (Connection, ProcessHandle)
initializeConnection ica = do
	fifos <- createFIFOs (icCreateFIFOs ica)
	ph <- launchFCEUX (icLaunch ica) { lfFIFOs = fifos }
	handles <- openFIFOs (icOpenFIFOs ica) { ofFIFOs = fifos }
	mconn <- connectToHandles (icConnect ica)
		{ cthServerToClient = serverToClient handles
		, cthClientToServer = clientToServer handles
		}
	case mconn of
		Just conn -> return (conn, ph)
		Nothing -> do
			pids <- getPid ph
			for_ pids $ \pid -> do
				signalProcess sigINT pid
				-- give it a polite second to finish up...
				code <- timeout (icKillTimeout ica) (waitForProcess ph)
				when (isNothing code) $ do
					-- ...then an impatient second...
					signalProcess sigTERM pid
					code <- timeout (icKillTimeout ica) (waitForProcess ph)
					-- ...then be a bit less polite
					when (isNothing code) (signalProcess sigKILL pid)
			fail "Version negotiation went south. Perhaps a diplomatic dialog should be opened between server and client authors."

data ConnectToHandlesArgs a = ConnectToHandlesArgs
	{ cthServerToClient :: a
	-- ^ The handle to read server messages from. You should not do anything
	-- with this handle after calling 'connectToHandles'. It will be closed for
	-- you if version negotiation fails.
	, cthClientToServer :: a
	-- ^ The handle to write client messages to. You should not do anything
	-- with this handle after calling 'connectToHandles'. It will be closed for
	-- you if version negotiation fails.
	, cthDeltaCallback :: GameDelta -> IO ()
	-- ^ A callback that will be invoked whenever something especially
	-- interesting happens. A thread is spawned for repeatedly invoking this
	-- callback. Defaults to silently throwing away the delta.
	, cthDiagnosticCallback :: Diagnostic -> IO ()
	-- ^ A callback that will be invoked whenever something weird or
	-- out-of-protocol happens. A thread is spawned for repeatedly invoking
	-- this callback. Defaults to printing to @stderr@.
	}

-- See [NOTE: (), not Default]
instance a ~ () => Default (ConnectToHandlesArgs a) where
	def = ConnectToHandlesArgs
		{ cthServerToClient = def
		, cthClientToServer = def
		, cthDeltaCallback = \_ -> return ()
		, cthDiagnosticCallback = hPrint stderr
		}

-- | Initialize a connection with a server. Blocks until version negotiation
-- with the server is finished; returns 'Nothing' if version negotiation fails.
connectToHandles :: ConnectToHandlesArgs Handle -> IO (Maybe Connection)
connectToHandles ctha = do
	chanDbg <- newTChanIO
	chanFromServer <- newTChanIO
	chanToServer <- newTChanIO
	threadToServer <- forkIO (writeMsgs (cthClientToServer ctha) chanDbg chanToServer)
	threadFromServer <- forkIO (readMsgs (cthServerToClient ctha) chanDbg chanFromServer)
	threadDbg <- forkIO . forever $ atomically (readTChan chanDbg) >>= cthDiagnosticCallback ctha
	goodVersion <- negotiateVersion chanDbg chanFromServer chanToServer
	if goodVersion
	then do
		refGameState <- newTMVarIO def
		forkIO (handleMessages chanDbg refGameState chanFromServer (cthDeltaCallback ctha))
		return (Just (Connection refGameState chanToServer))
	else do
		killThread threadToServer
		killThread threadFromServer
		-- threadDbg will get killed automatically once it's waiting on an
		-- empty TChan, and we don't really want or need to kill it earlier
		hClose (cthServerToClient ctha)
		hClose (cthClientToServer ctha)
		return Nothing

data FIFOs a = FIFOs
	{ tmpDir :: FilePath
	, serverToClient, clientToServer :: a
	} deriving (Eq, Ord, Read, Show)

data CreateFIFOsArgs = CreateFIFOsArgs
	{ cfTmpDir :: Maybe FilePath
	-- ^ For 'Nothing', we try the environment variables @XDG_RUNTIME_DIR@,
	-- @TMPDIR@, @TMP@, @TEMP@ in that order, falling back to @/tmp@ if all of
	-- them are unset.
	, cfPerms :: FileMode
	-- ^ Permissions to give the new pipes. Defaults to @0o600@ -- readable and
	-- writable by the current user.
	} deriving (Eq, Ord, Read, Show)

instance Default CreateFIFOsArgs where
	def = CreateFIFOsArgs
		{ cfTmpDir = Nothing
		, cfPerms = 0o600
		}

-- | Create some named pipes in a temporary directory suitable for use as
-- communication medium between server and client. It's polite to delete the
-- pipes and the directory when you're done. Throws exceptions left and right
-- if anything even slightly unsavory happens.
createFIFOs :: CreateFIFOsArgs -> IO (FIFOs FilePath)
createFIFOs cfa = do
	-- This pattern is complete because of the final return in asum's argument.
	Just tmpRoot <- runMaybeT . asum $
		[ MaybeT . traverse return $ cfTmpDir cfa
		, lookupEnvM "XDG_RUNTIME_DIR"
		, lookupEnvM "TMPDIR"
		, lookupEnvM "TMP"
		, lookupEnvM "TEMP"
		, return "/tmp"
		]
	dir <- createNewTemp tmpRoot 0
	let s2c = dir </> "s2c"
	    c2s = dir </> "c2s"
	createNamedPipe s2c (cfPerms cfa)
	createNamedPipe c2s (cfPerms cfa)
	return FIFOs
		{ tmpDir = dir
		, serverToClient = s2c
		, clientToServer = c2s
		}
	where
	lookupEnvM = MaybeT . lookupEnv
	createNewTemp root n = do
		let dir = root </> "maryodel-" <> show n
		(createDirectory dir >> return dir) `catch` \e ->
			if isAlreadyExistsError e
			then createNewTemp root (n+1)
			else throwIO e

data OpenFIFOsArgs a = OpenFIFOsArgs
	{ ofFIFOs :: a
	-- ^ Paths to the pipes to open (and the temporary directory we created to
	-- contain them).
	, ofRepeats :: Maybe Int
	-- ^ How many times to try opening the client-to-server pipe (which doesn't
	-- succeed until the server has opened its end). Use 'Nothing' to try
	-- forever. Defaults to @Just 10@.
	, ofTimeout :: Int
	-- ^ How many microseconds to wait each time we try to open the
	-- client-to-server pipe; default 0.1s.
	} deriving (Eq, Ord, Read, Show)

-- [NOTE: (), not Def]
-- In several places, we write something like
--
--     instance a ~ () => Default (FArgs a)
--
-- and you might wonder why it isn't this instead:
--
--     instance Default a => Default (FArgs a)
--
-- These types are default arguments to a function written in the
-- optional-named-argument style, so typical use will be something like this:
--
--     f def { fArg1 = foo, fArg5 = bar }
--
-- If we used `Default a => Default (FArgs a)`, then overwriting the fields
-- with type `a` in this way would be very frustrating: the type of the
-- original `def` would be ambiguous, able to choose among all `Default`
-- instances, but in a way that doesn't matter (since those fields are
-- completely ignored anyway).
--
-- Instead we use `a ~ () => Default (FArgs a)` to tell GHC exactly which
-- choice of `a` to use, to remove that ambiguity. This reduces the types that
-- `def` can inhabit, but mostly rules out types we probably don't plan to use
-- anyway.

-- See [NOTE: (), not Default]
instance a ~ () => Default (OpenFIFOsArgs a) where
	def = OpenFIFOsArgs
		{ ofFIFOs = def
		, ofRepeats = Just 10
		, ofTimeout = 100000
		}

-- | Opening pipes is a bit finicky. When opening the writing end you gotta
-- wait until there's somebody reading it. This opens both ends, giving the
-- server a second to show up and throwing an exception if it doesn't.
--
-- Deletes all the 'FilePath's it's passed.
openFIFOs :: OpenFIFOsArgs (FIFOs FilePath) -> IO (FIFOs Handle)
openFIFOs ofa = openBoth `finally` deleteAll where
	openBoth = do
		hs2c <- openFile (serverToClient fifos) ReadMode
		hc2s <- openWriteEnd (ofRepeats ofa)
		return fifos { serverToClient = hs2c, clientToServer = hc2s }

	openWriteEnd (Just 0) = fail $ "Couldn't open " <> clientToServer fifos <> " for writing. Perhaps the server isn't listening on that pipe?"
	openWriteEnd n = catch
		(openFile (clientToServer fifos) WriteMode)
		(\e -> if isDoesNotExistError e
		       then threadDelay (ofTimeout ofa) >> openWriteEnd (subtract 1 <$> n)
		       else throwIO e
		)

	deleteAll = return ()
		`finally` removeFile (serverToClient fifos)
		`finally` removeFile (clientToServer fifos)
		`finally` removeDirectory (tmpDir fifos)

	fifos = ofFIFOs ofa

data LaunchFCEUXArgs a b = LaunchFCEUXArgs
	{ lfRom :: a
	-- ^ The path to the Dr. Mario ROM
	, lfServer :: a
	-- ^ The path to the server protocol implementation lua script.
	, lfFIFOs :: b
	-- ^ The communication pipes, passed to the server on its @stdin@.
	, lfOutHandler :: Handle -> IO ()
	-- ^ What to do with FCEUX's @stdout@. The action will be run in its own
	-- thread, and is responsible for emptying the handle frequently. Defaults
	-- to silently throwing away everything.
	, lfErrHandler :: Handle -> IO ()
	-- ^ What to do with FCEUX's @stderr@. The action will be run in its own
	-- thread, and is responsible for emptying the handle frequently. Defaults
	-- to silently throwing away everything.
	}

-- See [NOTE: (), not Default]
instance (a ~ (), b ~ ()) => Default (LaunchFCEUXArgs a b) where
	def = LaunchFCEUXArgs
		{ lfRom = def
		, lfServer = def
		, lfFIFOs = def
		, lfOutHandler = forever . hGetLine
		, lfErrHandler = forever . hGetLine
		}

-- | Attempt to launch a Dr. Mario server in FCEUX. It informs the server about
-- the two communication pipes by passing them to its stdin.
launchFCEUX :: LaunchFCEUXArgs FilePath (FIFOs FilePath) -> IO ProcessHandle
launchFCEUX lfa = do
	(fceuxIn, fceuxOut, fceuxErr, fceuxPH) <- runInteractiveProcess "fceux" ["--loadlua", lfServer lfa, lfRom lfa] Nothing Nothing
	hPutStrLn fceuxIn (serverToClient (lfFIFOs lfa))
	hPutStrLn fceuxIn (clientToServer (lfFIFOs lfa))
	hClose fceuxIn
	forkIO $ lfOutHandler lfa fceuxOut
	forkIO $ lfErrHandler lfa fceuxErr
	return fceuxPH

currentGameState :: Connection -> IO GameState
currentGameState conn = atomically (readTMVar (refGameState conn)) >>= freezeGameState

-- | Start pressing buttons on a given frame number (and discard any previously
-- scheduled button presses on or after that frame). The callback supplied will
-- be called exactly once.
control :: Connection -> Word32 -> [R.ButtonPress] -> (Response () -> IO a) -> IO ()
control conn frame bps callback_ = do
	let callback = void . forkIO . void . callback_
	igs <- atomically $ takeTMVar (refGameState conn)
	case igs of
		IInProgress cbControl cbQueue cbState stateIDs youMode frame players -> do
			let (ident, cbControl') = fresh callback cbControl
			atomically $ do
				writeTChan (chanToServer conn) (R.Control ident frame bps)
				putTMVar (refGameState conn)
				         (IInProgress cbControl' cbQueue cbState stateIDs youMode frame players)
		_ -> do
			atomically $ putTMVar (refGameState conn) igs
			callback Discarded

-- | Start pressing buttons next time we enter control mode (and discard any
-- previously button presses previously scheduled for that transition). The
-- callback supplied will be called exactly once.
queue :: Connection -> [R.ButtonPress] -> (Response () -> IO a) -> IO ()
queue conn bps callback_ = do
	let callback = void . forkIO . void . callback_
	igs <- atomically $ takeTMVar (refGameState conn)
	case igs of
		IInProgress cbControl cbQueue cbState stateIDs youMode frame players -> do
			let (ident, cbQueue') = fresh callback cbQueue
			atomically $ do
				writeTChan (chanToServer conn) (R.Queue ident bps)
				putTMVar (refGameState conn)
				         (IInProgress cbControl cbQueue' cbState stateIDs youMode frame players)
		_ -> do
			atomically $ putTMVar (refGameState conn) igs
			callback Discarded

-- | Ask the server to send us the complete player state at a given future
-- moment. The callback supplied will be called exactly once.
requestState :: Connection -> R.StateRequestTime -> (Response (Map R.PlayerIdentifier PlayerState) -> IO a) -> IO ()
requestState conn t callback_ = do
	let callback = void . forkIO . void . callback_
	igs <- atomically $ takeTMVar (refGameState conn)
	case igs of
		IInProgress cbControl cbQueue cbState stateIDs youMode frame players -> atomically $ do
			let cbState' = M.insertWith (++) t [callback] cbState
			writeTChan (chanToServer conn) (R.RequestState t)
			putTMVar (refGameState conn)
			         (IInProgress cbControl cbQueue cbState' stateIDs youMode frame players)
		_ -> do
			atomically $ putTMVar (refGameState conn) igs
			callback Discarded

-- | Send the server some debug information to display however it likes. You
-- should keep the list under about a thousand elements to guarantee that it's
-- all processed by the server.
--
-- Returns 'True' if we actually sent the data, or 'False' if debug messages
-- are illegal in the current protocol state.
debug :: Connection -> [(Position, Cell)] -> IO Bool
debug conn info = do
	igs <- atomically $ takeTMVar (refGameState conn)
	atomically $ do
		putTMVar (refGameState conn) igs
		case igs of
			IInProgress{} -> True <$ writeTChan (chanToServer conn) (R.Debug info)
			_ -> return False

-- | The protocol's @version_proposal@ and @version_selection@ states are not
-- represented here; the connection initialization methods don't return until
-- version negotiation is finished.
data GameState
	= Setup !(Map R.PlayerIdentifier PlayerState)
	-- ^ The protocol's @game_setup_zero@ and @game_setup@ states are collapsed
	-- into 'Setup'. The 'Map' may have 'R.you' as a key.
	| InProgress !Word32 !PlayerState !(Map R.PlayerIdentifier PlayerState)
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
	= GameStarted !PlayerState !(Map R.PlayerIdentifier PlayerState)
	| State !R.PlayerIdentifier !PlayerState
	| Winner !R.PlayerIdentifier
	| Loser !R.PlayerIdentifier
	| ModeCleanup !R.PlayerIdentifier
	| ModeControl !R.PlayerIdentifier !PillContent
	| Frame !Word32
	| PillChanged !R.PlayerIdentifier !Pill
	| Speed !R.PlayerIdentifier !Word32
	| Garbage !R.PlayerIdentifier !(Map Int Color)
	| Quit -- ^ The server stopped sending messages. This is the end, folks.
	deriving (Eq, Ord, Read, Show)

data PlayerState = PlayerState
	{ dropRate :: !Word32 -- ^ How many frames can pass before a pill is forced to drop one row
	, pillLookahead :: !PillContent -- ^ The next pill the player will get to control
	, board :: !Board
	, mode :: !ModeState
	, dead :: !Bool -- ^ Has the player lost yet?
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

protocolStateFromModeState :: ModeState -> ProtocolState
protocolStateFromModeState Cleanup{} = CleanupState
protocolStateFromModeState Control{} = ControlState

-- | The internal version of the 'GameState', that includes extra stuff we need
-- to track to get the protocol right, mutable versions of the board for
-- efficiency, and so forth.
--
-- Extra fields come first.
data IGameState
	= ISetup !(Map R.PlayerIdentifier IPlayerState)
	| IInProgress !UnitCallbackMap !UnitCallbackMap !StateCallbackMap !(Set R.PlayerIdentifier) (Maybe YouMode)
	              !Word32 !(Map R.PlayerIdentifier IPlayerState)
	-- ^ Extra fields:
	--
	-- * callbacks for @control@ messages
	-- * callbacks for @queue@ messages
	-- * callbacks for @state@ messages
	-- * players we've seen a @state@ message for this frame
	-- * whether we've seen a @mode you@ message this frame
	--
	-- Unlike 'InProgress', we store @you@ in the final field and select it out
	-- when it's time to freeze.

instance Default IGameState where def = ISetup def

data YouMode = YouControl | YouCleanup deriving (Eq, Ord, Read, Show)

data Response a
	= Accept !a -- ^ The server accepted your request, and replied with the data included here
	| Far -- ^ The server rejected your request because it was for a time too far in the future
	| Old -- ^ The server rejected your request because it was about the past
	| Discarded -- ^ The server discarded your request because the conditions of the request didn't come up before the game ended, or we didn't even bother sending the request because it wasn't appropriate at that point in the protocol
	deriving (Eq, Ord, Read, Show, Functor)

type UnitCallbackMap = IdentifierMap (Response () -> IO ())
type StateCallbackMap = Map R.StateRequestTime [Response (Map R.PlayerIdentifier PlayerState) -> IO ()]

-- | A type for creating fresh 'R.Identifier's and associating them with
-- values.
data IdentifierMap a = IdentifierMap
	{ nextIdent :: !Word -- 2^30 identifiers ought to be enough for anybody
	, callbacks :: !(Map R.Identifier a)
	} deriving (Eq, Ord, Read, Show, Functor)

instance Default (IdentifierMap a) where def = IdentifierMap def def
instance Foldable IdentifierMap where foldMap f = foldMap f . callbacks

-- | Associate a fresh identifier with a given value.
fresh :: a -> IdentifierMap a -> (R.Identifier, IdentifierMap a)
fresh a m = (ident, m
	{ nextIdent = nextIdent m + 1
	, callbacks = M.insert ident a (callbacks m)
	})
	where
	ident = identifierFromWord (nextIdent m)

discharge :: IdentifierMap a -> R.Identifier -> Maybe (a, IdentifierMap a)
discharge m k = case M.updateLookupWithKey def k (callbacks m) of
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
	{ iCachedBoard :: TVar (Maybe Board)
	, iDropRate :: !Word32
	, iPillLookahead :: !PillContent
	, iBoard :: !IOBoard
	, iMode :: !ModeState
	, iDead :: !Bool
	}

-- | Pick out 'R.you', unless it ain't there, in which case pick out something
-- (anything, as long as it's a consistent choice), unless there's nothing
-- there, in which case 'Nothing' is there.
selectYou :: Map R.PlayerIdentifier a -> Maybe ((R.PlayerIdentifier, a), Map R.PlayerIdentifier a)
selectYou m = case M.updateLookupWithKey def R.you m of
	(Just v, m') -> Just ((R.you, v), m')
	_ -> M.minViewWithKey m

freezePlayerState :: IPlayerState -> IO PlayerState
freezePlayerState ips = do
	mFrozenBoard <- readTVarIO (iCachedBoard ips)
	frozenBoard <- case mFrozenBoard of
		Just frozenBoard -> return frozenBoard
		Nothing -> do
			frozenBoard <- mfreeze (iBoard ips)
			atomically $ writeTVar (iCachedBoard ips) (Just frozenBoard)
			return frozenBoard
	return PlayerState
		{ dropRate = iDropRate ips
		, pillLookahead = iPillLookahead ips
		, board = frozenBoard
		, mode = iMode ips
		, dead = iDead ips
		}

thawPlayerState :: PlayerState -> IO IPlayerState
thawPlayerState ps = do
	cachedBoard <- newTVarIO (Just (board ps))
	iBoard <- thaw (board ps)
	return IPlayerState
		{ iCachedBoard = cachedBoard
		, iDropRate = dropRate ps
		, iPillLookahead = pillLookahead ps
		, iBoard = iBoard
		, iMode = mode ps
		, iDead = dead ps
		}

freezeGameState :: IGameState -> IO GameState
freezeGameState (ISetup m) = Setup <$> traverse freezePlayerState m
freezeGameState (IInProgress _ _ _ _ _ frame players) = case selectYou players of
	Just ((_, ips), players') -> InProgress frame <$> freezePlayerState ips <*> traverse freezePlayerState players'
	_ -> fail "The impossible happened: freezeGameState was called with an empty player map."

handleMessages :: TChan Diagnostic -> TMVar IGameState -> TChan (Maybe R.ServerMessage) -> (GameDelta -> IO a) -> IO ()
handleMessages dbg refGameState msgs deltaCallback = go where
	go = do
		mmsg <- atomically $ readTChan msgs
		case mmsg of
			-- don't need to forkIO because we're not doing anything afterwards anyway
			Nothing -> void $ deltaCallback Quit
			Just msg -> do
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
				go

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

handleMessage (ISetup players@(selectYou -> Just ((you, ips), opponents))) (R.Frame frame) = do
	frozenYou <- freezePlayerState ips
	frozenOpponents <- traverse freezePlayerState opponents
	return
		( [MissingYou (M.insert you frozenYou frozenOpponents) | you /= R.you]
		, Just (GameStarted frozenYou frozenOpponents)
		, IInProgress def def def def def frame players
		)

handleMessage igs@(ISetup players) msg = return ([IllegalMessage protocolState msg], Nothing, igs) where
	protocolState = if M.size players == 0 then GameSetupZeroState else GameSetupState

handleMessage igs@(IInProgress cbControl cbQueue cbState stateIDs youMode frame players) msg = case msg of
	R.AcceptControl id -> triggerControlCallback id (Accept ())
	R.FarControl    id -> triggerControlCallback id Far
	R.OldControl    id -> triggerControlCallback id Old
	R.AcceptQueue   id -> triggerQueueCallback   id (Accept ())
	R.FarState frame   -> triggerStateCallback frame Far
	R.OldState frame   -> triggerStateCallback frame Old
	R.Frame frame -> return ([], Just (Frame frame), IInProgress cbControl cbQueue cbState def def frame players)

	R.State player drop lookahead b s -> case M.lookup player players of
		Just ips -> do
			let ps = PlayerState
			    	{ dropRate = drop
			    	, pillLookahead = lookahead
			    	, board = b
			    	, mode = case s of
			    		R.CleanupState -> Cleanup
			    		R.ControlState framesLeft pill -> Control (frame+framesLeft) pill
			    	, dead = iDead ips
			    	}
			ips' <- thawPlayerState ps
			let players' = M.insert player ips' players
			    stateIDs' = S.insert player stateIDs
			igs' <- handleStateCallbacks (IInProgress cbControl cbQueue cbState stateIDs' youMode frame players')
			return ([], Just (State player ps), igs')
		Nothing -> illegalM

	R.Winner player -> do
		discardAll cbControl cbQueue cbState
		return ( [UnknownPlayer player | M.notMember player players]
		       , Just (Winner player)
		       , ISetup def
		       )

	R.Loser player -> do
		let living = M.foldlWithKey' (\acc k v -> acc + if k == player || iDead v then 0 else 1) 0 players
		case M.updateLookupWithKey (\_ ips -> Just ips { iDead = True }) player players of
			_ | living < 2 -> do
				discardAll cbControl cbQueue cbState
				let diagnostics = [UnknownPlayer player | M.notMember player players]
				return (diagnostics, Just (Loser player), ISetup def)
			(Nothing, _) -> illegalM
			(_, players') -> do
				igs' <- handleStateCallbacks (IInProgress cbControl cbQueue cbState stateIDs youMode frame players')
				return ([], Just (Loser player), igs')

	R.ModeCleanup player -> case M.lookup player players of
		Just ips -> case iMode ips of
			Control _ pill -> do
				success <- mplace (iBoard ips) pill
				when success . atomically $ writeTVar (iCachedBoard ips) Nothing
				let players' = M.insert player ips { iMode = Cleanup } players
				    youMode' = if player == R.you then Just YouCleanup else youMode
				igs' <- if player == R.you
					then handleStateCallbacks (IInProgress cbControl cbQueue cbState stateIDs (Just YouCleanup) frame players')
					else return (IInProgress cbControl cbQueue cbState stateIDs youMode frame players')
				return ( [IllegalPill player pill | not success]
				       , Just (ModeCleanup player)
				       , igs'
				       )
			Cleanup -> return ([DoubleCleanup player], Nothing, igs)
		Nothing -> illegalM

	R.ModeControl player lookahead -> case M.updateLookupWithKey (\_ -> Just . enterControlMode lookahead) player players of
		(Nothing, _) -> illegalM
		(_, players') -> do
			igs' <- if player == R.you
				then handleStateCallbacks (IInProgress cbControl cbQueue cbState stateIDs (Just YouControl) frame players')
				else return (IInProgress cbControl cbQueue cbState stateIDs youMode frame players')
			return ([], Just (ModeControl player lookahead), igs')

	R.Pill player pill -> return $ case M.updateLookupWithKey (\_ -> Just . setPill pill) player players of
		(Nothing, _) -> illegal
		(_, players') -> ([], Just (PillChanged player pill), IInProgress cbControl cbQueue cbState stateIDs youMode frame players')

	R.Speed player dropRate -> return $ case M.updateLookupWithKey (\_ ips -> Just ips { iDropRate = dropRate }) player players of
		(Nothing, _) -> illegal
		(_, players') -> ([], Just (Speed player dropRate), IInProgress cbControl cbQueue cbState stateIDs youMode frame players')

	R.Garbage player cs -> case M.lookup player players of
		Nothing -> illegalM
		Just ips -> do
			success <- mgarbage (iBoard ips) cs
			if success then do
				atomically $ writeTVar (iCachedBoard ips) Nothing
				return ([], Just (Garbage player cs), igs)
			else illegalM

	R.ProposeVersion{} -> illegalM
	R.RequestVersion{} -> illegalM

	where
	triggerControlCallback id resp = case discharge cbControl id of
		Just (callback, cbControl') -> do
			callback resp
			return ([], Nothing, IInProgress cbControl' cbQueue cbState stateIDs youMode frame players)
		Nothing -> return ([UnknownID msg], Nothing, igs)

	triggerQueueCallback id resp = case discharge cbQueue id of
		Just (callback, cbQueue') -> do
			callback resp
			return ([], Nothing, IInProgress cbControl cbQueue' cbState stateIDs youMode frame players)
		Nothing -> return ([UnknownID msg], Nothing, igs)

	triggerStateCallback frame resp = case M.lookup (R.AtFrame frame) cbState of
		Just (cb:cbs) -> do
			cb resp
			let cbState' = M.insert (R.AtFrame frame) cbs cbState
			return ([], Nothing, IInProgress cbControl cbQueue cbState' stateIDs youMode frame players)
		_ -> return ([UnknownFrame frame (void resp)], Nothing, igs)

	setPill pill' ips = case iMode ips of
		Cleanup -> ips
		Control dropFrame pill -> ips
			{ iMode = Control
				(if y (bottomLeftPosition pill) == y (bottomLeftPosition pill')
				 then dropFrame
				 else frame + iDropRate ips
				)
				pill'
			}

	enterControlMode lookahead ips = ips
		{ iMode = Control (frame + iDropRate ips) (Pill lookahead Position { x = 3, y = 15 })
		}

	protocolState = case selectYou players of
		Just ((_, you), _) -> protocolStateFromModeState (iMode you)
		_ -> ControlState -- WTF, how this happen

	illegal = ([IllegalMessage protocolState msg], Nothing, igs)
	illegalM = return illegal

handleStateCallbacks :: IGameState -> IO IGameState
handleStateCallbacks igs@(IInProgress cbControl cbQueue cbState stateIDs youMode frame players)
	| M.null (M.withoutKeys livePlayers stateIDs) = do
		cbState' <- return cbState
			>>= handleTime (R.AtFrame frame)
			>>= handleTime R.Immediately
			>>= case youMode of
				Nothing -> return
				Just YouControl -> handleTime R.NextControlMode
				Just YouCleanup -> handleTime R.NextCleanupMode
		return (IInProgress cbControl cbQueue cbState' stateIDs youMode frame players)
	where
	livePlayers = M.filter (not . iDead) players
	handleTime t s = case M.updateLookupWithKey def t s of
		(Nothing, _) -> return s
		(Just cbs, s') -> do
			frozen <- traverse freezePlayerState livePlayers
			traverse_ ($Accept frozen) cbs
			return s'
handleStateCallbacks igs = return igs

discardAll :: UnitCallbackMap -> UnitCallbackMap -> StateCallbackMap -> IO ()
discardAll cbControl cbQueue cbState = do
	traverse_ ($Discarded) cbControl
	traverse_ ($Discarded) cbQueue
	traverse_ (traverse_ ($Discarded)) cbState
