{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Dr.Mario.Model.Internal
	( Color(..), colorChar, parseColor
	, Shape(..), shapeChar, parseShape
	, Cell(..)
	, Board(..)
	, emptyBoard
	, MBoard(..)
	) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.Default
import Data.Foldable
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Primitive.ByteArray (setByteArray)
import Data.Word
import qualified Data.Aeson.Encoding              as E
import qualified Data.ByteString.Builder          as B
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as DVG
import qualified Data.Vector.Generic.Mutable.Base as DVGMB
import qualified Data.Vector.Primitive.Mutable    as DVPM
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as MV

data Color = Red | Yellow | Blue deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | A horizontal pill has a 'West' shape to the left of an 'East' shape, and a
-- vertical pill has a 'North' shape above a 'South' shape.
data Shape = Virus | Disconnected | North | South | East | West deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Cell = Empty | Occupied !Color !Shape deriving (Eq, Ord, Read, Show)

instance Hashable Color where hashWithSalt = hashUsing fromEnum

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

colorChar :: Color -> Char
colorChar = \case
	Blue -> 'b'
	Red -> 'r'
	Yellow -> 'y'

parseColor :: Parser Color -> Char -> Parser Color
parseColor err = \case
	'b' -> pure Blue
	'r' -> pure Red
	'y' -> pure Yellow
	_ -> err

instance ToJSON Color where
	toJSON = toJSON . colorChar
	toEncoding = toEncoding . colorChar
	toJSONList = toJSON . map colorChar
	toEncodingList = toEncoding . map colorChar

instance FromJSON Color where
	parseJSON v = parseJSON v >>= parseColor err where
		err = typeMismatch "Color (\"b\", \"r\", or \"y\")" v
	parseJSONList v = parseJSON v >>= traverse (parseColor err) where
		err = typeMismatch "[Color] (string with only 'b', 'r', and 'y' in it)" v

shapeChar :: Shape -> Char
shapeChar = \case
	Virus -> 'x'
	Disconnected -> 'o'
	North -> '∩'
	South -> '∪'
	East -> '⊃'
	West -> '⊂'

parseShape :: Parser Shape -> Char -> Parser Shape
parseShape err = \case
	'x' -> pure Virus
	'o' -> pure Disconnected
	'∩' -> pure North
	'∪' -> pure South
	'⊃' -> pure East
	'⊂' -> pure West
	_ -> err

instance ToJSON Shape where
	toJSON = toJSON . shapeChar
	toEncoding = toEncoding . shapeChar
	toJSONList = toJSON . map shapeChar
	toEncodingList = toEncoding . map shapeChar

instance FromJSON Shape where
	parseJSON v = parseJSON v >>= parseShape err where
		err = typeMismatch "Shape (\"x\", \"o\", \"∩\", \"∪\", \"⊃\", or \"⊂\")" v
	parseJSONList v = parseJSON v >>= traverse (parseShape err) where
		err = typeMismatch "[Shape] (string with only \'x\', \'o\', \'∩\', \'∪\', \'⊃\', and \'⊂\' in it)" v

cellShowS :: Cell -> String -> String
cellShowS cell s = case cell of
	Empty -> ' ':' ':s
	Occupied color shape -> colorChar color : shapeChar shape : s

parseCell :: (forall a. Parser a) -> Char -> Char -> Parser Cell
parseCell err ' ' ' ' = pure Empty
parseCell err c s = pure Occupied <*> parseColor err c <*> parseShape err s

instance ToJSON Cell where
	toJSON = toJSON . flip cellShowS ""
	toEncoding = toEncoding . flip cellShowS ""
	toJSONList = toJSON . foldr cellShowS ""
	toEncodingList = toEncoding . foldr cellShowS ""

instance FromJSON Cell where
	parseJSON v = parseJSON v >>= \case
		[c, s] -> parseCell err c s
		_ -> err
		where
		err :: Parser a
		err = typeMismatch "Cell (color character followed by shape character)" v
	parseJSONList v = parseJSON v >>= go where
		go (c:s:rest) = pure (:) <*> parseCell err c s <*> go rest
		go [] = pure []
		go _ = err

		err :: Parser a
		err = typeMismatch "[Cell] (string of alternating colors and shapes)" v

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

data MBoard s = MBoard
	{ mwidth, mheight :: !Int
	, mcells :: !(MV.MVector s Cell)
	}
