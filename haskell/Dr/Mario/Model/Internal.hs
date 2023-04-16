{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Dr.Mario.Model.Internal
	( Color(..)
	, Shape(..)
	, Orientation(..)
	, Rotation(..)
	, Cell(..)
	, Board(..)
	, emptyBoard
	, MBoard(..)
	, SingleCharJSON(..), SingleChar(..)
	, (~>), parseSingleCharOr
	, ParserSource(..)
	) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.Coerce
import Data.Default
import Data.Foldable
import Data.Functor.Contravariant
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.List
import Data.Ix
import Data.Map (Map)
import Data.Primitive.ByteArray (setByteArray)
import Data.Typeable
import Data.Word
import GHC.Stack
import qualified Data.Aeson.Encoding              as E
import qualified Data.ByteString.Builder          as B
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as DVG
import qualified Data.Vector.Generic.Mutable.Base as DVGMB
import qualified Data.Vector.Primitive.Mutable    as DVPM
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as MV

data Color = Red | Yellow | Blue
	deriving (Bounded, Enum, Eq, Ord, Read, Show)
	deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via SingleCharJSON Color

-- | A horizontal pill has a 'West' shape to the left of an 'East' shape, and a
-- vertical pill has a 'North' shape above a 'South' shape.
data Shape = Virus | Disconnected | North | South | East | West
	deriving (Bounded, Enum, Eq, Ord, Read, Show)
	deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via SingleCharJSON Shape

data Cell = Empty | Occupied !Color !Shape deriving (Eq, Ord, Read, Show)

data Orientation = Horizontal | Vertical
	deriving (Bounded, Enum, Ix, Eq, Ord, Read, Show)
	deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via SingleCharJSON Orientation

data Rotation = Clockwise | Counterclockwise
	deriving (Bounded, Enum, Eq, Ord, Read, Show)
	deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via SingleCharJSON Rotation

instance Hashable Color       where hashWithSalt = hashUsing fromEnum
instance Hashable Shape       where hashWithSalt = hashUsing fromEnum
instance Hashable Orientation where hashWithSalt = hashUsing fromEnum
instance Hashable Rotation    where hashWithSalt = hashUsing fromEnum

{-# INLINE decodeCell #-}
decodeCell :: Word8 -> Cell
decodeCell 0xff = Empty
decodeCell w = Occupied color shape where
	color = case w .&. 0b11 of
		0 -> Red
		1 -> Yellow
		_ -> Blue
	shape = case w .&. 0b11100 of
		0  -> Virus
		4  -> Disconnected
		8  -> North
		12 -> South
		16 -> East
		_  -> West

{-# INLINE encodeCell #-}
encodeCell :: Cell -> Word8
encodeCell Empty = 0xff
encodeCell (Occupied color shape) = colorWord .|. shapeWord where
	colorWord = case color of
		Red    -> 0
		Yellow -> 1
		Blue   -> 2
	shapeWord = case shape of
		Virus        ->  0
		Disconnected ->  4
		North        ->  8
		South        -> 12
		East         -> 16
		West         -> 20

newtype instance U.MVector s Cell = MVCell (U.MVector s Word8)
newtype instance U.Vector    Cell =  VCell (U.Vector    Word8)

instance DVGMB.MVector U.MVector Cell where
	{-# INLINE basicLength #-}
	basicLength (MVCell v) = DVGMB.basicLength v
	{-# INLINE basicUnsafeSlice #-}
	basicUnsafeSlice i j (MVCell v) = MVCell (DVGMB.basicUnsafeSlice i j v)
	{-# INLINE basicOverlaps #-}
	basicOverlaps (MVCell v) (MVCell v') = DVGMB.basicOverlaps v v'
	{-# INLINE basicUnsafeNew #-}
	basicUnsafeNew n = MVCell <$> DVGMB.basicUnsafeNew n
	{-# INLINE basicInitialize #-}
	basicInitialize (MVCell (U.MV_Word8 (DVPM.MVector i n ba))) = setByteArray ba i n (encodeCell Empty)
	{-# INLINE basicUnsafeRead #-}
	basicUnsafeRead (MVCell v) i = decodeCell <$> DVGMB.basicUnsafeRead v i
	{-# INLINE basicUnsafeWrite #-}
	basicUnsafeWrite (MVCell v) i = DVGMB.basicUnsafeWrite v i . encodeCell

instance DVG.Vector U.Vector Cell where
	{-# INLINE basicUnsafeFreeze #-}
	basicUnsafeFreeze (MVCell v) = VCell <$> DVG.basicUnsafeFreeze v
	{-# INLINE basicUnsafeThaw #-}
	basicUnsafeThaw (VCell v) = MVCell <$> DVG.basicUnsafeThaw v
	{-# INLINE basicLength #-}
	basicLength (VCell v) = DVG.basicLength v
	{-# INLINE basicUnsafeSlice #-}
	basicUnsafeSlice i j (VCell v) = VCell (DVG.basicUnsafeSlice i j v)
	{-# INLINE basicUnsafeIndexM #-}
	basicUnsafeIndexM (VCell v) i = decodeCell <$> DVG.basicUnsafeIndexM v i

instance SingleChar Orientation where
	toChar = \case
		Horizontal -> '↔'
		Vertical -> '↕'
	fromChar = tail [undefined
		, "↔-" ~> Horizontal
		, "↕|" ~> Vertical
		]

instance SingleChar Rotation where
	toChar = \case
		Clockwise -> '↻'
		Counterclockwise -> '↺'
	fromChar = tail [undefined
		, "↻cC" ~> Clockwise
		, "↺wW" ~> Counterclockwise
		]

instance SingleChar Color where
	toChar = \case
		Blue -> 'b'
		Red -> 'r'
		Yellow -> 'y'
	fromChar = tail [undefined
		, "bB" ~> Blue
		, "rR" ~> Red
		, "yY" ~> Yellow
		]

instance SingleChar Shape where
	toChar = \case
		Virus -> 'x'
		Disconnected -> 'o'
		North -> '∩'
		South -> '∪'
		East -> '⊃'
		West -> '⊂'
	fromChar = tail [undefined
		, "x"   ~> Virus
		, "o"   ~> Disconnected
		, "∩^"  ~> North
		, "∪vV" ~> South
		, "⊃>"  ~> East
		, "⊂<"  ~> West
		]

cellShowS :: Cell -> String -> String
cellShowS cell s = case cell of
	Empty -> ' ':' ':s
	Occupied color shape -> toChar color : toChar shape : s

parseCell :: (forall a. Parser a) -> Char -> Char -> Parser Cell
parseCell err ' ' ' ' = pure Empty
parseCell err c s = pure Occupied <*> parseSingleCharOr err c <*> parseSingleCharOr err s

cellFromSource :: ParserSource src => src -> Parser Cell
cellFromSource src = srcString src >>= \case
	[c, s] -> parseCell err c s
	_ -> err
	where
	err :: Parser a
	err = mismatch "Cell (color character followed by shape character, or two spaces)" src

cellsFromSource :: ParserSource src => src -> Parser [Cell]
cellsFromSource src = srcString src >>= go where
	go (c:s:rest) = pure (:) <*> parseCell err c s <*> go rest
	go [] = pure []
	go _ = err

	err :: Parser a
	err = mismatch "[Cell] (string of alternating colors and shapes, or two consecutive spaces for empty cells)" src

instance ToJSON Cell where
	toJSON = toJSON . flip cellShowS ""
	toEncoding = toEncoding . flip cellShowS ""
	toJSONList = toJSON . foldr cellShowS ""
	toEncodingList = toEncoding . foldr cellShowS ""

instance FromJSON Cell where
	parseJSON = cellFromSource
	parseJSONList = cellsFromSource

instance ToJSONKey Cell where
	toJSONKey = contramap (flip cellShowS "") toJSONKey
	toJSONKeyList = contramap (foldr cellShowS "") toJSONKey

instance FromJSONKey Cell where
	fromJSONKey = FromJSONKeyTextParser cellFromSource
	fromJSONKeyList = FromJSONKeyTextParser cellsFromSource

instance U.Unbox Cell

data Board = Board
	{ height :: !Int
	, cells :: !(V.Vector (U.Vector Cell))
	} deriving (Eq, Ord, Read, Show)

emptyBoard
	:: Int -- ^ width
	-> Int -- ^ height
	-> Board
emptyBoard w h = Board h (V.replicate w (U.replicate h Empty))

instance Default Board where def = emptyBoard 8 16

transposedCells :: Board -> V.Vector (V.Vector Cell)
transposedCells Board { height = h, cells = cs } = V.generate h $ \r -> (U.! (h-1-r)) <$> cs

-- N.B. we store the *width*, not the height, because we're transposing the cells
boardToStorage :: Board -> (Int, V.Vector String)
boardToStorage b = (V.length (cells b), foldr cellShowS "" <$> transposedCells b)

unsafeStorageToBoard :: (Int, V.Vector [Cell]) -> Board
unsafeStorageToBoard (w, rows_) = Board
	{ height = h
	, cells = V.generate w $ \x -> U.generate h $ \y -> rows V.! (h-1-y) V.! x
	} where
	h = V.length rows
	rows = V.fromListN w <$> rows_

instance ToJSON Board where
	toJSON = toJSON . boardToStorage
	toEncoding = toEncoding . boardToStorage

instance FromJSON Board where
	parseJSON v = do
		storage@(w, rows) <- parseJSON v
		unless
			(all (\row -> length row == w) rows)
			(typeMismatch "Board" v)
		pure (unsafeStorageToBoard storage)

instance ToJSONKey Board
instance FromJSONKey Board

data MBoard s = MBoard
	{ mwidth, mheight :: !Int
	, mcells :: !(MV.MVector s Cell)
	}

class ParserSource src where
	mismatch :: String -> src -> Parser a
	srcString :: src -> Parser String
	srcChar :: src -> Parser Char

instance ParserSource Value where
	mismatch = typeMismatch
	srcString = parseJSON
	srcChar = parseJSON

instance ParserSource T.Text where
	mismatch desc t = fail $ "expected " ++ desc ++ ", but saw " ++ T.unpack t
	srcString = pure . T.unpack
	srcChar t = case T.unpack t of
		[c] -> pure c
		s -> fail $ "expected a single-character string, but got " ++ s

-- | Laws:
--
-- * @(toChar a, a) `elem` map (\(c, a, _) -> (c, a)) fromChar@ for all @a@
-- * @fromCharMap = fromList [(cAlt, a) | (c, a, cs) <- fromChar, cAlt <- c:cs]@
-- * @(\s -> s == nub s) (fromChar >>= \(c, _, cs) -> c:cs)@
--
-- If you define 'fromCharMap', you might want to mark it to be inlined; the
-- JSON instances for 'SingleCharJSON' should propagate this inlining so that
-- the typeclass polymorphism doesn't come back to bite us. (Fingers crossed.)
class SingleChar a where
	{-# MINIMAL toChar, fromChar #-}
	toChar :: a -> Char
	fromChar :: [(Char, a, [Char])]

	-- | Used for errors.
	typeName :: String
	default typeName :: (Typeable a, SingleChar a) => String
	typeName = show (typeRep ([]@a))

	-- So this is like, kinda unfortunate, in that because it is
	-- class-polymorphic, this Map might be recomputed at every use. To combat
	-- this, we aggressively add INLINE annotations up the call tree, only
	-- stopping when we get to a caller that's monomorphic enough to specialize
	-- things nicely. (In this case, that is mostly DerivingVia instances.)
	{-# INLINE fromCharMap #-}
	fromCharMap :: Map Char a
	fromCharMap = M.fromList [(cAlt, a) | (c, a, cs) <- fromChar, cAlt <- c:cs]

(~>) :: String -> a -> (Char, a, [Char])
(c:cs) ~> a = (c, a, cs)

describe :: forall a. SingleChar a => String
describe = typeName @a ++ " (" ++ alternatives ++ ")" where
	alternatives = case fromChar @a of
		[] -> "there are no valid encodings of this uninhabited type"
		[(c, _, cs)] -> case cs of
			[]   -> "the exact string " ++ quoted c
			[c'] -> "the string " ++ quoted c ++ " or its alias " ++ quoted c'
			_    -> "the string " ++ quoted c ++ " or one of its aliases, " ++ orFList quoted cs
		alts -> "a string with one of the following characters: " ++ describeManyAlternatives alts

	quoted c = ['"', c, '"']

describeList :: forall a. SingleChar a => String
describeList = "[" ++ typeName @a ++ "]" ++ " (" ++ alternatives ++ ")" where
	alternatives = case fromChar @a of
		[] -> "an empty string"
		[(c, _, cs)] -> "a string containing some number of copies of " ++ [c] ++ case cs of
			[] -> ""
			[c'] -> " or its alias " ++ [c']
			_ -> " or its aliases, " ++ orFList pure cs
		alts -> "a string containing only the following characters: " ++ describeManyAlternatives alts

describeManyAlternatives :: [(Char, a, [Char])] -> String
describeManyAlternatives = orFList $ \(c, _, cs) -> c : case cs of
	[] -> ""
	[c'] -> " (or its alias, " ++ [c'] ++ ")"
	_ -> " (or one of its aliases, " ++ orList (map pure cs) ++ ")"

orFList :: HasCallStack => (a -> String) -> [a] -> String
orFList f = orList . map f

orList :: HasCallStack => [String] -> String
orList [] = error "orList: no alternatives"
orList [s] = s
orList [s, s'] = s ++ " or " ++ s'
orList ss = intercalate ", " (init ss ++ ["or " ++ last ss])

{-# INLINE parseSingleCharOr #-}
parseSingleCharOr :: SingleChar a => Parser a -> Char -> Parser a
parseSingleCharOr err c = maybe err pure (M.lookup c fromCharMap)

{-# INLINE parseSingleChar #-}
parseSingleChar :: forall src a. (ParserSource src, SingleChar a) => src -> Parser a
parseSingleChar src = srcChar src >>= parseSingleCharOr (mismatch (describe @a) src)

{-# INLINE parseSingleCharList #-}
parseSingleCharList :: forall src a. (ParserSource src, SingleChar a) => src -> Parser [a]
parseSingleCharList src = srcString src >>= traverse (parseSingleCharOr (mismatch (describeList @a) src))

newtype SingleCharJSON a = SingleCharJSON { unSingleCharJSON :: a }

instance SingleChar a => SingleChar (SingleCharJSON a) where
	{-# INLINE toChar #-}
	toChar = coerce (toChar @a)

	{-# INLINE fromChar #-}
	fromChar = coerce (fromChar @a)

	{-# INLINE typeName #-}
	typeName = typeName @a

	{-# INLINE fromCharMap #-}
	fromCharMap = coerce (fromCharMap @a)

instance SingleChar a => ToJSON (SingleCharJSON a) where
	toJSON = toJSON . toChar
	toEncoding = toEncoding . toChar
	toJSONList = toJSON . map toChar
	toEncodingList = toEncoding . map toChar

instance SingleChar a => ToJSONKey (SingleCharJSON a) where
	toJSONKey = contramap ((:[]) . toChar) toJSONKey
	toJSONKeyList = contramap (map toChar) toJSONKey

instance SingleChar a => FromJSON (SingleCharJSON a) where
	{-# INLINE parseJSON #-}
	parseJSON = parseSingleChar
	{-# INLINE parseJSONList #-}
	parseJSONList = parseSingleCharList

instance SingleChar a => FromJSONKey (SingleCharJSON a) where
	{-# INLINE fromJSONKey #-}
	fromJSONKey = FromJSONKeyTextParser parseSingleChar
	{-# INLINE fromJSONKeyList #-}
	fromJSONKeyList = FromJSONKeyTextParser parseSingleCharList
