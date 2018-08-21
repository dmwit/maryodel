{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals #-}
module Dr.Mario.Protocol.Raw
	( Identifier, identifier, unsafeIdentifier, getIdentifier
	, PlayerIdentifier, playerIdentifier, you
	, protocolVersion
	, maxIdentifierLength, messageSeparator, componentSeparator
	, Button(..)
	, ButtonAction(..)
	, ButtonPress(..)
	, ServerMessage(..), ModeState(..)
	, ClientMessage(..), StateRequestTime(..)
	, Protocol(..), Parser, ParseWarning(..)
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Bits
import Data.ByteString (ByteString)
import Data.List (transpose)
import Data.Foldable
import Data.Map (Map)
import Data.Word
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

-- Only skip the Pill data constructor that conflicts with ServerMessage(Pill)
-- (but still import the Pill type constructor); then we'll import that data
-- constructor qualified below so we can still use it if we need to.
import Dr.Mario.Model hiding (Pill(Pill))
import Dr.Mario.Model (Pill)
import Dr.Mario.Model.Internal
import qualified Dr.Mario.Model as Model

-- | An identifier is any sequence of 40 bytes with no ASCII spaces or
-- newlines.
newtype Identifier = Identifier { _getIdentifier :: ByteString }
	deriving (Eq, Ord, Read, Show)

-- | An 'Identifier' where the bytes for @"you"@ are considered special.
type PlayerIdentifier = Identifier

maxIdentifierLength :: Int
maxIdentifierLength = 40

messageSeparator, componentSeparator :: Word8
messageSeparator = 10
componentSeparator = 32

-- | Returns nothing if the 'ByteString' is too long or has invalid bytes.
identifier :: ByteString -> Maybe Identifier
identifier bs = do
	guard (BS.length bs <= maxIdentifierLength)
	guard (messageSeparator   `BS.notElem` bs)
	guard (componentSeparator `BS.notElem` bs)
	return (Identifier bs)

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p v = v <$ guard (p v)

-- | In addition to the checks made by 'identifier', this ensures that its
-- argument is not the special identifier 'you' used by servers to indicate the
-- current client.
playerIdentifier :: ByteString -> Maybe PlayerIdentifier
playerIdentifier = identifier >=> ensure (you/=)

-- | Does not check the length or validity of the contained bytes.
unsafeIdentifier :: ByteString -> Identifier
unsafeIdentifier = Identifier

getIdentifier :: Identifier -> ByteString
getIdentifier = _getIdentifier

-- | Intended for internal use only. Doesn't check that characters are in the
-- single-byte range, doesn't check the length, doesn't check the validity of
-- the bytes produced.
unsafeStringIdentifier :: String -> Identifier
unsafeStringIdentifier = Identifier . BS.pack . map (fromIntegral . fromEnum)

-- | The version of the protocol currently supported by this library.
protocolVersion :: Identifier
protocolVersion = unsafeStringIdentifier "0-statefix-oldfarfix-boundfix-posfix"

-- | The special identifier used by servers to indicate the player associated
-- with the current client.
you :: PlayerIdentifier
you = unsafeStringIdentifier "you"

data Button = L | R | D | A | B deriving (Bounded, Enum, Eq, Ord, Read, Show)
data ButtonAction = Toggle | Close | Open deriving (Bounded, Enum, Eq, Ord, Read, Show)
newtype ButtonPress = ButtonPress { getAtomicButtonPresses :: Map Button ButtonAction } deriving (Eq, Ord, Read, Show)

data ModeState
	= CleanupState
	| ControlState !Word32 !Pill
	deriving (Eq, Ord, Read, Show)

data StateRequestTime
	= AtFrame !Word32
	| Immediately
	| NextCleanupMode
	| NextControlMode
	deriving (Eq, Ord, Read, Show)

-- TODO: parseComponent = parseComponentSeparator *> parse
-- | Messages that the server can send.
data ServerMessage
	= AcceptControl !Identifier
	| AcceptQueue !Identifier
	| FarControl !Identifier
	| FarState !Word32
	| Frame !Word32
	| Garbage !PlayerIdentifier !(Map Int Color)
	| Loser !PlayerIdentifier
	| ModeCleanup !PlayerIdentifier
	| ModeControl !PlayerIdentifier !PillContent
	| OldControl !Identifier
	| OldState !Word32
	| Pill !PlayerIdentifier !Pill
	| Players !Word32
	| ProposeVersion !Identifier
	| RequestVersion
	| Speed !PlayerIdentifier !Word32
	| State !PlayerIdentifier !Word32 !PillContent !Board !ModeState
	| Winner !PlayerIdentifier
	deriving (Eq, Ord, Read, Show)

-- | Messages the client can send.
data ClientMessage
	= Control !Identifier !Word32 [ButtonPress]
	| Debug [(Position, Cell)]
	| Queue !Identifier [ButtonPress]
	| RequestState !StateRequestTime
	| Version !Identifier
	deriving (Eq, Ord, Read, Show)

-- | 'Parser's occasionally try to return something sensible even when they see
-- illegal message formats. When they do this, they report some information
-- about the invalid value they saw and the guess about what a reasonable
-- corresponding valid value would be. In each constructor, invalid values come
-- first, and the corrected values come last.
--
-- If you wanted to write an application which hewed very strictly to the
-- protocol, you would be within your rights to throw an error if you ever
-- received bytes which, when parsed, produced a 'ParseWarning'.
data ParseWarning
	= TruncatedLongIdentifier !ByteString !Identifier
	| IgnoredLeadingZeros !ByteString
	| IntegerOverflowed !Integer !Word32
	| XPositionOverflowed !Word32 !Int
	| YPositionOverflowed !Word32 !Int
	| AtomicButtonPressesIgnored [(Button, ButtonAction)] !ButtonPress
	| InvalidGarbageCorrected [Int] [Cell] !(Map Int Color)
	deriving (Eq, Ord, Read, Show)

type Parser = WriterT [ParseWarning] A.Parser

complain :: ParseWarning -> Parser ()
complain = tell . (:[])

complainIf :: Bool -> ParseWarning -> Parser ()
complainIf p w = when p (complain w)

parseComponentSeparator :: Parser ()
parseComponentSeparator = () <$ lift (A.word8 componentSeparator)

parseMessageSeparator :: Parser ()
parseMessageSeparator = () <$ lift (A.word8 messageSeparator)

parseChar :: Char -> Parser ()
parseChar char = () <$ (lift . A.word8 . toEnum . fromEnum) char

parseVerb :: String -> Parser ()
parseVerb v = () <$ (lift . A.string . BS.pack . map (toEnum . fromEnum)) v

parseComponent :: String -> Parser ()
parseComponent = parseVerb

class Protocol a where
	parse :: Parser a

instance Protocol Identifier where
	parse = do
		bs <- lift $ A.takeWhile (\c -> c /= messageSeparator && c /= componentSeparator)
		let ident = Identifier (BS.take 40 bs)
		complainIf (BS.length bs > maxIdentifierLength)
		           (TruncatedLongIdentifier bs ident)
		return ident

instance Protocol Word32 where
	parse = do
		digits <- lift (A.takeWhile1 (\c -> 48 <= c && c <= 57) A.<?> "decimal digits")
		complainIf (BS.length digits > 1 && BS.head digits == 48)
		           (IgnoredLeadingZeros digits)
		let integer = BS.foldl' (\n digit -> n*10 + toInteger digit - 48) 0 digits
		    word32 = fromInteger integer
		complainIf (integer >= 2^32) (IntegerOverflowed integer word32)
		return word32

instance Protocol Position where
	parse = do
		x_ <- parse
		parseComponentSeparator
		y_ <- parse
		let x = fromIntegral (x_ `mod` xMax)
		    y = fromIntegral (y_ `mod` yMax)
		complainIf (x_>=xMax) (XPositionOverflowed x_ x)
		complainIf (y_>=yMax) (YPositionOverflowed y_ y)
		return (Position x y)
		where
		xMax = 8
		yMax = 16

instance Protocol Button where
	parse = asum
		[ L <$ parseChar 'l'
		, R <$ parseChar 'r'
		, D <$ parseChar 'd'
		, A <$ parseChar 'a'
		, B <$ parseChar 'b'
		]

instance Protocol ButtonAction where
	parse = asum
		[ Close <$ parseChar '+'
		, Open  <$ parseChar '-'
		, return Toggle
		]

instance Protocol (Button, ButtonAction) where
	parse = liftA2 (flip (,)) parse parse

instance Protocol ButtonPress where
	parse = (ButtonPress . uncurry M.singleton <$> parse) <|> do
		parseChar '('
		ps <- many parse
		parseChar ')'
		let atomicButtonPresses = M.fromList ps
		    buttonPress = ButtonPress atomicButtonPresses
		complainIf (not . null . drop (length atomicButtonPresses) $ ps)
		           (AtomicButtonPressesIgnored ps buttonPress)
		return buttonPress

instance Protocol Cell where
	parse = do
		w <- lift (A.satisfy (\w -> 97 <= w && w <= 119 && w `notElem` [104,108,112,116]))
		let color = case w .&. 0b11 of
		    	1 -> Red
		    	2 -> Yellow
		    	3 -> Blue
		    shape = case w .&. 0b11100 of
		    	0  -> Virus
		    	4  -> Disconnected
		    	8  -> South
		    	12 -> North
		    	16 -> West
		    	20 -> East
		return $ case w of
			100 -> Empty
			_ -> Occupied color shape

instance Protocol PillContent where
	parse = do
		bottomLeft <- parse
		otherPosition <- parse
		case (bottomLeft, otherPosition) of
			(Occupied blc South, Occupied opc North) -> return PillContent
				{ orientation = Vertical
				, bottomLeftColor = blc
				, otherColor = opc
				}
			(Occupied blc West, Occupied opc East) -> return PillContent
				{ orientation = Horizontal
				, bottomLeftColor = blc
				, otherColor = opc
				}
			_ -> fail "expected a pill, but saw two cells with empty or mismatched shapes"

instance Protocol Pill where
	parse = do
		p <- parse
		parseComponentSeparator
		c <- parse
		return Model.Pill { content = c, bottomLeftPosition = p }

instance Protocol Board where
	parse = do
		-- TODO: Ouch. These list manipulations are due to the protocol being
		-- row-major order and the model being column-major order. Should we
		-- change the internal representation of the model to make this more
		-- efficient? Or maybe construct an MBoard and freeze it so that we can
		-- just walk the list once and do bit manipulations to get the right
		-- index into the mutable vector?
		cs <- transpose . reverse <$> replicateM 16 (replicateM 8 parse)
		return Board
			{ height = 16
			, cells = V.fromListN 8 (map (U.fromListN 16) cs)
			}

instance Protocol StateRequestTime where
	parse = asum
		[ AtFrame <$ parseComponentSeparator <*> parse
		, NextControlMode <$ parseComponentSeparator <* parseComponent "control"
		, NextCleanupMode <$ parseComponentSeparator <* parseComponent "cleanup"
		, return Immediately
		]

instance Protocol ClientMessage where
	parse = asum
		[ do
			parseVerb "control"
			parseComponentSeparator
			id <- parse
			parseComponentSeparator
			frame <- parse
			parseComponentSeparator
			bs <- many parse
			return (Control id frame bs)
		, do
			parseVerb "debug"
			pcs <- many $ do
				parseComponentSeparator
				p <- parse
				parseComponentSeparator
				c <- parse
				return (p, c)
			return (Debug pcs)
		, do
			parseVerb "queue"
			parseComponentSeparator
			id <- parse
			parseComponentSeparator
			bs <- many parse
			return (Queue id bs)
		, do
			parseVerb "request-state"
			RequestState <$> parse
		, do
			parseVerb "version"
			parseComponentSeparator
			Version <$> parse
		] <* parseMessageSeparator

instance Protocol ModeState where
	parse = asum
		[ CleanupState <$ parseComponent "cleanup"
		, ControlState <$ parseComponent "control" <* parseComponentSeparator <*> parse <* parseComponentSeparator <*> parse
		]

instance Protocol ServerMessage where
	parse = asum
		[ parseSingle AcceptControl "accept-control"
		, parseSingle AcceptQueue "accept-queue"
		, parseSingle FarControl "far-control"
		, parseSingle FarState "far-state"
		, parseSingle Frame "frame"
		, do
			parseVerb "garbage"
			parseComponentSeparator
			player <- parse
			rawColumns <- lift (A.takeWhile (\w -> 48 <= w && w <= 55))
			parseComponentSeparator
			rawCells <- many parse
			let columns = [fromIntegral (c - 48) | c <- BS.unpack rawColumns]
			    garbagePairs =
			    	[ (column, color)
			    	| (column, rawCell) <- zip columns rawCells
			    	, Just color <- [color rawCell]
			    	]
			    numCells = length rawCells
			    numColumns = BS.length rawColumns
			    cellInvalid cell = shape cell /= Just Disconnected
			    garbage = M.fromList garbagePairs
			complainIf (  numColumns /= numCells
			           || numColumns /= length garbage
			           || any cellInvalid rawCells
			           )
			           (InvalidGarbageCorrected columns rawCells garbage)
			return (Garbage player garbage)
		, parseSingle Loser "loser"
		, do
			parseVerb "mode"
			parseComponentSeparator
			player <- parse
			parseComponentSeparator
			asum
				[ do
					parseComponent "cleanup"
					return (ModeCleanup player)
				, do
					parseComponent "control"
					parseComponentSeparator
					pill <- parse
					return (ModeControl player pill)
				]
		, parseSingle OldControl "old-control"
		, parseSingle OldState "old-state"
		, do
			parseVerb "pill"
			parseComponentSeparator
			player <- parse
			parseComponentSeparator
			pill <- parse
			return (Pill player pill)
		, parseSingle Players "players"
		, parseSingle ProposeVersion "propose-version"
		, RequestVersion <$ parseVerb "request-version"
		, do
			parseVerb "speed"
			parseComponentSeparator
			player <- parse
			parseComponentSeparator
			speed <- parse
			return (Speed player speed)
		, do
			parseVerb "state"
			parseComponentSeparator
			player <- parse
			parseComponentSeparator
			dropFrames <- parse
			parseComponentSeparator
			pill <- parse
			parseComponentSeparator
			board <- parse
			parseComponentSeparator
			modeState <- parse
			return (State player dropFrames pill board modeState)
		, parseSingle Winner "winner"
		] <* parseMessageSeparator
		where
		parseSingle constructor verb = do
			parseVerb verb
			parseComponentSeparator
			constructor <$> parse
