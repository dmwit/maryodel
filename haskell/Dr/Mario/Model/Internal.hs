{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Dr.Mario.Model.Internal
	( Color(..)
	, Shape(..)
	, Cell(..)
	, Board(..)
	) where

import Data.Bits
import Data.Primitive.ByteArray (setByteArray)
import Data.Word
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as DVG
import qualified Data.Vector.Generic.Mutable.Base as DVGMB
import qualified Data.Vector.Primitive.Mutable    as DVPM
import qualified Data.Vector.Unboxed              as U

data Color = Red | Yellow | Blue deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Shape = Virus | Disconnected | North | South | East | West deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Cell = Empty | Occupied !Color !Shape deriving (Eq, Ord, Read, Show)

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

instance U.Unbox Cell

data Board = Board
	{ height :: !Int
	, cells :: !(V.Vector (U.Vector Cell))
	} deriving (Eq, Ord, Read, Show)
