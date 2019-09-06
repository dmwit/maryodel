{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals #-}
module Dr.Mario.Protocol.Raw
	( -- * 'ByteString' conversions
      Protocol(..), Parser, parse, ParseWarning(..), Printer, PrintWarning(..)
	  -- * Messages
	, ServerMessage(..), ClientMessage(..)
      -- * 'Identifier's
	, Identifier, identifier, unsafeIdentifier, getIdentifier
	, PlayerIdentifier, playerIdentifier, you
	, protocolVersion
	, messageSeparator, componentSeparator
	  -- * Limits
	, identifierSize
	, buttonPressSize
	, messageSize
	, xSize, ySize
	  -- * Other supporting types
	, ModeState(..)
	, StateRequestTime(..)
	, Button(..)
	, ButtonAction(..)
	, ButtonPress(..)
	, Word32
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.List (transpose)
import Data.Foldable
import Data.Map (Map)
import Data.Maybe
import Data.Word
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Map.Strict as M
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

-- Only skip pp and the Pill data constructor that conflicts with
-- ServerMessage(Pill) (but still import the Pill type constructor); then we'll
-- import that data constructor qualified below so we can still use it if we
-- need to.
import Dr.Mario.Model hiding (Pill(Pill), pp)
import Dr.Mario.Model (Pill)
import Dr.Mario.Model.Internal
import qualified Dr.Mario.Model as Model

-- | An identifier is any sequence of 40 bytes with no ASCII spaces or
-- newlines.
newtype Identifier = Identifier { _getIdentifier :: ByteString }
	deriving (Eq, Ord, Read, Show)

-- | An 'Identifier' where the bytes for @"you"@ are considered special.
type PlayerIdentifier = Identifier

-- | The maximum length for 'Identifier's.
identifierSize :: Int
identifierSize = 40

messageSeparator, componentSeparator :: Word8
messageSeparator = 10
componentSeparator = 32

-- | Returns nothing if the 'ByteString' is too long or has invalid bytes.
identifier :: ByteString -> Maybe Identifier
identifier bs = do
	guard (BS.length bs <= identifierSize)
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
protocolVersion = unsafeStringIdentifier "1"

-- | The special identifier used by servers to indicate the player associated
-- with the current client.
you :: PlayerIdentifier
you = unsafeStringIdentifier "you"

data Button = L | R | D | A | B deriving (Bounded, Enum, Eq, Ord, Read, Show)
data ButtonAction = Toggle | Close | Open deriving (Bounded, Enum, Eq, Ord, Read, Show)
newtype ButtonPress = ButtonPress { getAtomicButtonPresses :: Map Button ButtonAction } deriving (Eq, Ord, Read, Show)

data ModeState
	= CleanupState
	| ControlState !Word32 !Pill -- ^ how many frames before the pill is forced to drop one row, what the pill is
	deriving (Eq, Ord, Read, Show)

data StateRequestTime
	= AtFrame !Word32
	| Immediately
	| NextCleanupMode
	| NextControlMode
	deriving (Eq, Ord, Read, Show)

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

complainParser :: ParseWarning -> Parser ()
complainParser = tell . (:[])

complainIfParser :: Bool -> ParseWarning -> Parser ()
complainIfParser p w = when p (complainParser w)

parseComponentSeparator :: Parser ()
parseComponentSeparator = () <$ lift (A.word8 componentSeparator)

parseMessageSeparator :: Parser ()
parseMessageSeparator = () <$ lift (A.word8 messageSeparator)

parseChar :: Char -> Parser ()
parseChar char = () <$ (lift . A.word8 . toEnum . fromEnum) char

parseLiteral :: String -> Parser ()
parseLiteral v = () <$ (lift . A.string . BS.pack . map (toEnum . fromEnum)) v

-- | Some instances of 'Protocol' are capable of holding invalid values
-- (notably 'Position's and garbage). 'Printer's do their best to handle these
-- invalid values, and always produce something that is valid within the
-- protocol anyway. When they do this, they report some information about the
-- invalid value they saw and their guess about what a reasonable corresponding
-- valid value to send would be. In each constructor, invalid values come
-- first, and the corrected values come last.
--
-- Some messages are variable-length, and these messages have length limits
-- imposed by the protocol. It is allowed to violate these limits, but servers
-- may ignore parts of the message that extend beyond the limit. In such cases,
-- the 'Printer' produces messages that exceed the limits and emits a warning
-- containing the longest prefix of the data that the server is guaranteed to
-- use.
--
-- Your application should strive to never pass 'Printer's values which produce
-- warnings.
data PrintWarning
	= XPositionWrapped !Int !Word32
	| YPositionWrapped !Int !Word32
	| WrongBoardSizeCorrected !Board -- TODO: report something about the corrected value (maybe report a corrected Board?)
	| LongButtonPressSequenceRetained [ButtonPress] [ButtonPress]
	-- For simplicity, PossiblyLongDebugSequenceRetained just reports the long
	-- debug sequence, not the guaranteed-received prefix, and may sometimes
	-- give a false positive for messages near the limit.
	-- TODO: Eliminate false positives, and report the actual guaranteed-received prefix.
	| PossiblyLongDebugSequenceRetained [(Position, Cell)]
	| OutOfBoundsGarbageIgnored !(Map Int Color) !(Map Int Color)
	deriving (Eq, Ord, Read, Show)

-- PrintResult is not exported, so only use it in the type signatures of values that aren't exported.
type PrintResult = ([PrintWarning], Builder)
type Printer a = a -> ([PrintWarning], Builder)

liftPP :: Printer Builder
liftPP b = ([], b)

-- Could give it the type Printer PrintWarning, but that would really give the
-- wrong impression, since the warning doesn't get put in the "printing" part
-- but in the "warning" part.
complainPrinter :: PrintWarning -> PrintResult
complainPrinter w = ([w], mempty)

complainIfPrinter :: Bool -> PrintWarning -> PrintResult
complainIfPrinter b w = ([w | b], mempty)

ppComponentSeparator :: PrintResult
ppComponentSeparator = liftPP . B.word8 $ componentSeparator

ppMessageSeparator :: PrintResult
ppMessageSeparator = liftPP . B.word8 $ messageSeparator

ppChar :: Printer Char
ppChar = liftPP . B.word8 . toEnum . fromEnum

ppLiteral :: Printer String
ppLiteral s = ([], foldMap (B.word8 . toEnum . fromEnum) s)

class Protocol a where
	parseT :: Parser a
	pp :: Printer a

parse :: Protocol a => A.Parser (a, [ParseWarning])
parse = runWriterT parseT

parseComponent :: Protocol a => Parser a
parseComponent = parseComponentSeparator *> parseT

parse0 :: (                      ) => (          r) -> String -> Parser r
parse1 :: (Protocol a            ) => (a      -> r) -> String -> Parser r
parse2 :: (Protocol a, Protocol b) => (a -> b -> r) -> String -> Parser r
parse3 ::
	(Protocol a, Protocol b, Protocol c) =>
	(a -> b -> c -> r) ->
	String -> Parser r
parse4 ::
	(Protocol a, Protocol b, Protocol c, Protocol d) =>
	(a -> b -> c -> d -> r) ->
	String -> Parser r
parse5 ::
	(Protocol a, Protocol b, Protocol c, Protocol d, Protocol e) =>
	(a -> b -> c -> d -> e -> r) ->
	String -> Parser r

parse0 constructor verb = constructor <$ parseLiteral verb
parse1 constructor verb = constructor <$ parseLiteral verb <*> parseComponent
parse2 constructor verb = constructor <$ parseLiteral verb <*> parseComponent <*> parseComponent
parse3 constructor verb = constructor <$ parseLiteral verb <*> parseComponent <*> parseComponent <*> parseComponent
parse4 constructor verb = constructor <$ parseLiteral verb <*> parseComponent <*> parseComponent <*> parseComponent <*> parseComponent
parse5 constructor verb = constructor <$ parseLiteral verb <*> parseComponent <*> parseComponent <*> parseComponent <*> parseComponent <*> parseComponent

ppComponent :: Protocol a => Printer a
ppComponent a = ppComponentSeparator <> pp a

pp0 :: (                      ) => String ->           PrintResult
pp1 :: (Protocol a            ) => String -> a ->      PrintResult
pp2 :: (Protocol a, Protocol b) => String -> a -> b -> PrintResult
pp3 ::
	(Protocol a, Protocol b, Protocol c) =>
	String ->
	a -> b -> c ->
	PrintResult
pp4 ::
	(Protocol a, Protocol b, Protocol c, Protocol d) =>
	String ->
	a -> b -> c -> d ->
	PrintResult
pp5 ::
	(Protocol a, Protocol b, Protocol c, Protocol d, Protocol e) =>
	String ->
	a -> b -> c -> d -> e ->
	PrintResult

pp0 verb           = ppLiteral verb
pp1 verb a         = ppLiteral verb <> ppComponent a
pp2 verb a b       = ppLiteral verb <> ppComponent a <> ppComponent b
pp3 verb a b c     = ppLiteral verb <> ppComponent a <> ppComponent b <> ppComponent c
pp4 verb a b c d   = ppLiteral verb <> ppComponent a <> ppComponent b <> ppComponent c <> ppComponent d
pp5 verb a b c d e = ppLiteral verb <> ppComponent a <> ppComponent b <> ppComponent c <> ppComponent d <> ppComponent e

instance Protocol Identifier where
	parseT = do
		bs <- lift $ A.takeWhile (\c -> c /= messageSeparator && c /= componentSeparator)
		let ident = Identifier (BS.take 40 bs)
		complainIfParser
			(BS.length bs > identifierSize)
			(TruncatedLongIdentifier bs ident)
		return ident

	pp (Identifier bs) = liftPP $ B.byteString bs

instance Protocol Word32 where
	parseT = do
		digits <- lift (A.takeWhile1 (\c -> 48 <= c && c <= 57) A.<?> "decimal digits")
		complainIfParser
			(BS.length digits > 1 && BS.head digits == 48)
			(IgnoredLeadingZeros digits)
		let integer = BS.foldl' (\n digit -> n*10 + toInteger digit - 48) 0 digits
		    word32 = fromInteger integer
		complainIfParser (integer >= 2^32) (IntegerOverflowed integer word32)
		return word32

	pp 0 = liftPP $ B.word8 48
	pp n = liftPP $ go n where
		go 0 = mempty
		go n = let (q, r) = quotRem n 10 in go q <> B.word8 (48 + fromIntegral r)

-- | In the current version of the protocol, boards always have exactly the
-- same size.
xSize :: Num a => a

-- | In the current version of the protocol, boards always have exactly the
-- same size.
ySize :: Num a => a

xSize = 8
ySize = 16

instance Protocol Position where
	parseT = do
		x_ <- parseT
		y_ <- parseComponent
		let x = fromIntegral (x_ `mod` xSize)
		    y = fromIntegral (y_ `mod` ySize)
		complainIfParser (x_>=xSize) (XPositionOverflowed x_ x)
		complainIfParser (y_>=ySize) (YPositionOverflowed y_ y)
		return (Position x y)

	pp (Position x__ y__)
		=  complainIfPrinter (x__/=x_) (XPositionWrapped x__ x)
		<> complainIfPrinter (y__/=y_) (YPositionWrapped y__ y)
		<> pp x
		<> ppComponentSeparator
		<> pp y
		where
		x_ = x__ `mod` xSize
		y_ = y__ `mod` ySize
		x = fromIntegral x_
		y = fromIntegral y_

instance Protocol Button where
	parseT = asum
		[ L <$ parseChar 'l'
		, R <$ parseChar 'r'
		, D <$ parseChar 'd'
		, A <$ parseChar 'a'
		, B <$ parseChar 'b'
		]

	pp L = ppChar 'l'
	pp R = ppChar 'r'
	pp D = ppChar 'd'
	pp A = ppChar 'a'
	pp B = ppChar 'b'

instance Protocol ButtonAction where
	parseT = asum
		[ Close <$ parseChar '+'
		, Open  <$ parseChar '-'
		, return Toggle
		]

	pp Close  = ppChar '+'
	pp Open   = ppChar '-'
	pp Toggle = mempty

instance Protocol (Button, ButtonAction) where
	parseT = liftA2 (flip (,)) parseT parseT
	pp (b, a) = pp a <> pp b

instance Protocol ButtonPress where
	parseT = (ButtonPress . uncurry M.singleton <$> parseT) <|> do
		parseChar '('
		ps <- many parseT
		parseChar ')'
		let atomicButtonPresses = M.fromList ps
		    buttonPress = ButtonPress atomicButtonPresses
		complainIfParser
			(not . null . drop (length atomicButtonPresses) $ ps)
			(AtomicButtonPressesIgnored ps buttonPress)
		return buttonPress

	pp (ButtonPress ps)
		=  (if M.size ps == 1 then mempty else ppChar '(')
		<> foldMap pp (M.toList ps)
		<> (if M.size ps == 1 then mempty else ppChar ')')

instance Protocol Cell where
	parseT = do
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

	pp Empty = liftPP (B.word8 100)
	pp (Occupied color shape) = liftPP . B.word8 $ upperBits .|. colorBits .|. shapeBits where
		upperBits = 96
		colorBits = case color of
			Red    -> 1
			Yellow -> 2
			Blue   -> 3
		shapeBits = case shape of
			Virus        ->  0
			Disconnected ->  4
			South        ->  8
			North        -> 12
			West         -> 16
			East         -> 20

instance Protocol PillContent where
	parseT = do
		bottomLeft <- parseT
		otherPosition <- parseT
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

	pp pc =  pp (Occupied (bottomLeftColor pc) bottomLeftShape)
	      <> pp (Occupied (     otherColor pc)      otherShape)
		where
		(bottomLeftShape, otherShape) = case orientation pc of
			Horizontal -> (West , East )
			Vertical   -> (South, North)

instance Protocol Pill where
	parseT = do
		p <- parseT
		c <- parseComponent
		return Model.Pill { content = c, bottomLeftPosition = p }

	pp (Model.Pill { content = c, bottomLeftPosition = p })
		=  pp p
		<> ppComponent c

instance Protocol Board where
	parseT = do
		-- TODO: Ouch. These list manipulations are due to the protocol being
		-- row-major order and the model being column-major order. Should we
		-- change the internal representation of the model to make this more
		-- efficient? Or maybe construct an MBoard and freeze it so that we can
		-- just walk the list once and do bit manipulations to get the right
		-- index into the mutable vector?
		cs <- transpose . reverse <$> replicateM ySize (replicateM xSize parseT)
		return Board
			{ height = ySize
			, cells = V.fromListN xSize (map (U.fromListN ySize) cs)
			}

	pp board
		= complainIfPrinter
			(width board /= xSize || height board /= ySize)
			(WrongBoardSizeCorrected board)
		<> go 0 (ySize-1)
		where
		go x y
			| x == xSize && y == 0 = mempty
			| x == xSize = go 0 (y-1)
			| otherwise = pp (fromMaybe Empty (get board (Position x y))) <> go (x+1) y

instance Protocol StateRequestTime where
	parseT = asum
		[ AtFrame <$> parseComponent
		, NextControlMode <$ parseComponentSeparator <* parseLiteral "control"
		, NextCleanupMode <$ parseComponentSeparator <* parseLiteral "cleanup"
		, return Immediately
		]

	pp (AtFrame n) = ppComponent n
	pp NextControlMode = ppComponentSeparator <> ppLiteral "control"
	pp NextCleanupMode = ppComponentSeparator <> ppLiteral "cleanup"
	pp Immediately = mempty

-- | Only certain instances @X@ of 'Protocol' should be promotable to @[X]@
-- instances of 'Protocol'. In particular, we shouldn't double-promote from @X@
-- to @[[X]]@, since then we'd be nesting calls to 'many' which is unsafe for
-- attoparsec parsers.
--
-- This also gives a little bit of safety: we only include 'Repeatable'
-- instances when the repetition of a type's protocol format is clearly
-- splittable in the right way, which helps us catch type errors if we try to
-- parse a list using 'parseT' for an unintended type.
--
-- You should not create instances of 'Repeatable' yourself.
class Repeatable a
instance Repeatable Cell
instance Repeatable ButtonPress
instance Repeatable (Position, Cell)
instance Repeatable ClientMessage
instance Repeatable ServerMessage

-- | The following 'Repeatable' instances exist: 'Cell'; 'ButtonPress';
-- @('Position', 'Cell')@; 'ClientMessage'; 'ServerMessage'.
instance (Protocol a, Repeatable a) => Protocol [a] where
	parseT = many parseT
	pp xs = foldMap pp xs

instance Protocol (Position, Cell) where
	parseT = liftA2 (,) parseComponent parseComponent
	pp (p, c) = ppComponent p <> ppComponent c

-- | The longest sequence of 'ButtonPress'es that the protocol guarantees will
-- be accepted as-is by your communication partner.
buttonPressSize :: Int
buttonPressSize = 624

-- | The longest message length that the protocol guarantees will be accepted
-- as-is by your communication partner.
messageSize :: Int
messageSize = 8192

instance Protocol ClientMessage where
	parseT = asum
		[ parse3 Control "control"
		, parse1 Debug "debug"
		, parse2 Queue "queue"
		, RequestState <$ parseLiteral "request-state" <*> parseT
		, parse1 Version "version"
		] <* parseMessageSeparator

	pp m = go m <> ppMessageSeparator where
		go (Control ident frame buttons)
			=  complainIfPrinter
				(length buttons > buttonPressSize)
				(LongButtonPressSequenceRetained buttons (take buttonPressSize buttons))
			<> pp3 "control" ident frame buttons
		go (Debug pcs)
			= complainIfPrinter
				(length pcs >= (messageSize - length "debug ") `div` length " 7 15 a")
				(PossiblyLongDebugSequenceRetained pcs)
			<> pp1 "debug" pcs
		go (Queue ident buttons)
			= complainIfPrinter
				(length buttons > buttonPressSize)
				(LongButtonPressSequenceRetained buttons (take buttonPressSize buttons))
			<> pp2 "queue" ident buttons
		go (RequestState time) = ppLiteral "request-state" <> pp time
		go (Version v) = pp1 "version" v

instance Protocol ModeState where
	parseT = asum
		[ parse0 CleanupState "cleanup"
		, parse2 ControlState "control"
		]

	pp CleanupState = pp0 "cleanup"
	pp (ControlState frames pill) = pp2 "control" frames pill

instance Protocol ServerMessage where
	parseT = asum
		[ parse1 AcceptControl "accept-control"
		, parse1 AcceptQueue "accept-queue"
		, parse1 FarControl "far-control"
		, parse1 FarState "far-state"
		, parse1 Frame "frame"
		, do
			parseLiteral "garbage"
			player <- parseComponent
			parseComponentSeparator
			rawColumns <- lift (A.takeWhile (\w -> 48 <= w && w <= 55))
			rawCells <- parseComponent
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
			complainIfParser
				(  numColumns /= numCells
				|| numColumns /= length garbage
				|| any cellInvalid rawCells
				)
				(InvalidGarbageCorrected columns rawCells garbage)
			return (Garbage player garbage)
		, parse1 Loser "loser"
		, do
			parseLiteral "mode"
			player <- parseComponent
			parseComponentSeparator
			asum
				[ parse0 (ModeCleanup player) "cleanup"
				, parse1 (ModeControl player) "control"
				]
		, parse1 OldControl "old-control"
		, parse1 OldState "old-state"
		, parse2 Pill "pill"
		, parse1 ProposeVersion "propose-version"
		, parse0 RequestVersion "request-version"
		, parse2 Speed "speed"
		, parse5 State "state"
		, parse1 Winner "winner"
		] <* parseMessageSeparator

	pp m = go m <> ppMessageSeparator where
		go (AcceptControl ident) = pp1 "accept-control" ident
		go (AcceptQueue ident) = pp1 "accept-queue" ident
		go (FarControl ident) = pp1 "far-control" ident
		go (FarState n) = pp1 "far-state" n
		go (Frame n) = pp1 "frame" n
		go (Garbage player garbage_)
			=  complainIfPrinter
				(M.size garbage_ /= M.size garbage)
				(OutOfBoundsGarbageIgnored garbage_ garbage)
			<> pp1 "garbage" player
			<> ppComponentSeparator
			<> M.foldMapWithKey (\column _ -> pp (fromIntegral column :: Word32)) garbage
			<> ppComponentSeparator
			<> M.foldMapWithKey (\_ color -> pp (Occupied color Disconnected)) garbage
			where
			garbage = M.takeWhileAntitone (xSize>) . M.dropWhileAntitone (0>) $ garbage_
		go (Loser player) = pp1 "loser" player
		go (ModeCleanup player) = pp1 "mode" player <> ppComponentSeparator <> pp0 "cleanup"
		go (ModeControl player pill) = pp1 "mode" player <> ppComponentSeparator <> pp1 "control" pill
		go (OldControl ident) = pp1 "old-control" ident
		go (OldState n) = pp1 "old-state" n
		go (Pill player pill) = pp2 "pill" player pill
		go (ProposeVersion ident) = pp1 "propose-version" ident
		go (RequestVersion) = pp0 "request-version"
		go (Speed player n) = pp2 "speed" player n
		go (State player dropFrames pill board modeState) = pp5 "state" player dropFrames pill board modeState
		go (Winner player) = pp1 "winner" player
