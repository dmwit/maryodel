{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DrMario
	( Color(..)
	, Shape(..)
	, Cell(..), color
	, Orientation(..)
	, Position(..)
	, Direction, left, right, down
	, Rotation(..)
	, Pill(..), otherPosition
	, Board
	, emptyBoard
	, width, height
	, get, getColor, unsafeGet
	, move, rotate, place, garbage
	, pp
	, MBoard
	, thaw, mfreeze, munsafeFreeze
	, memptyBoard
	, mwidth, mheight
	, minfect
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bits ((.&.), (.|.))
import Data.Default.Class
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Primitive.ByteArray (setByteArray)
import Data.Set (Set)
import Data.Word
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as DVG
import qualified Data.Vector.Generic.Mutable.Base as DVGMB
import qualified Data.Vector.Primitive.Mutable    as DVPM
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as M
import qualified System.Console.ANSI              as ANSI

data Color = Red | Yellow | Blue deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Shape = Virus | Disconnected | North | South | East | West deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Cell = Empty | Occupied !Color !Shape deriving (Eq, Ord, Read, Show)

color :: Cell -> Maybe Color
color (Occupied color shape) = Just color
color Empty = Nothing

data Orientation = Horizontal | Vertical deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | Uses the math convention: the bottom of a 'Board' is at 'y'=0, the top at some positive 'y'.
data Position = Position { x, y :: !Int } deriving (Eq, Ord, Read, Show)
data Direction = Direction { dx, dy :: !Int } deriving (Eq, Ord, Show)
data Rotation = Clockwise | Counterclockwise deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Pill = Pill
	{ orientation :: !Orientation
	, bottomLeftColor, otherColor :: !Color
	, bottomLeftPosition :: !Position
	} deriving (Eq, Ord, Read, Show)

otherPosition :: Pill -> Position
otherPosition pill = unsafeMove dir pos where
	dir = case orientation pill of
		Horizontal -> right
		Vertical   -> up
	pos = bottomLeftPosition pill

data Board = Board
	{ height :: !Int
	, cells :: !(V.Vector (U.Vector Cell))
	} deriving (Eq, Ord, Read, Show)

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

perpendicular :: Orientation -> Orientation
perpendicular Horizontal = Vertical
perpendicular Vertical   = Horizontal

left, right, up, down :: Direction
left  = Direction (-1)  0
right = Direction   1   0
up    = Direction   0   1
down  = Direction   0 (-1)

width :: Board -> Int
width = V.length . cells

emptyBoard
	:: Int -- ^ width
	-> Int -- ^ height
	-> Board
emptyBoard w h = Board h (V.replicate w (U.replicate h Empty))

instance Default Board where def = emptyBoard 8 16

get :: Board -> Position -> Maybe Cell
get b p = (V.!? x p) >=> (U.!? y p) $ cells b

getColor :: Board -> Position -> Maybe Color
getColor b = get b >=> color

-- | Does not check that the position is in bounds.
unsafeGet :: Board -> Position -> Cell
unsafeGet b p = cells b `V.unsafeIndex` x p `U.unsafeIndex` y p

-- | For debugging purposes only. Not particularly efficient.
pp :: Board -> String
pp b = unlines
	[ concat
		[ ppCell (unsafeGet b Position { x = x, y = y })
		| x <- [0 .. width b-1]
		]
	| y <- [height b-1, height b-2 .. 0]
	] ++ ANSI.setSGRCode []
	where
	ppCell Empty = " "
	ppCell (Occupied color shape) = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull (ppColor color)] ++ ppShape shape

	ppColor Red    = ANSI.Red
	ppColor Yellow = ANSI.Yellow
	ppColor Blue   = ANSI.Cyan

	ppShape Virus        = "☻"
	ppShape Disconnected = "o"
	ppShape North        = "^"
	ppShape South        = "v"
	ppShape East         = ">"
	ppShape West         = "<"

-- | Does absolutely no checks that the new position is sensible in any way.
unsafeMove :: Direction -> Position -> Position
unsafeMove d p = Position
	{ x = x p + dx d
	, y = y p + dy d
	}

-- | Move a pill in the given direction, if nothing is blocking it and it would
-- remain in bounds. Otherwise return 'Nothing'.
move :: Board -> Pill -> Direction -> Maybe Pill
move board pill dir =
	if get board pos1 == Just Empty && (get board pos2 == Just Empty || y pos2 == height board)
	then Just pill { bottomLeftPosition = pos1 }
	else Nothing
	where
	pos1 = unsafeMove dir (bottomLeftPosition pill)
	pos2 = unsafeMove (case orientation pill of Vertical -> up; Horizontal -> right) pos1

-- | Does no checks that the rotated pill would be in bounds, overlapping with
-- something on the board, etc. Just does it.
unsafeRotate :: Pill -> Rotation -> Pill
unsafeRotate pill rot = case rot of
	Clockwise -> case or of
		Horizontal -> swapped
		Vertical   -> rotated
	Counterclockwise -> case or of
		Horizontal -> rotated
		Vertical   -> swapped
	where
	or = orientation pill
	rotated = pill { orientation = perpendicular or }
	swapped = rotated { bottomLeftColor = otherColor pill, otherColor = bottomLeftColor pill }

-- | Rotate a pill in the given direction. 'Vertical' to 'Horizontal' rotations
-- kick left once if necessary. There are no vertical kicks. If there is no
-- room for the rotated version even after checking the kick position, returns
-- 'Nothing'.
rotate :: Board -> Pill -> Rotation -> Maybe Pill
rotate board pill rot = case orientation pill of
	Horizontal -> case neighbor up of
		Nothing    -> rotated -- at the top of the board
		Just Empty -> rotated
		_          -> Nothing
	Vertical -> case (neighbor left, getBL id, neighbor right) of
		(_, Just Empty, Just Empty) -> rotated
		(Just Empty, Just Empty, _) -> kickedAndRotated
		_                           -> Nothing
	where
	getBL f  = get board (f (bottomLeftPosition pill))
	neighbor = getBL . unsafeMove
	rotated  = Just (unsafeRotate pill rot)
	kicked   = pill { bottomLeftPosition = unsafeMove left (bottomLeftPosition pill) }
	kickedAndRotated = Just (unsafeRotate kicked rot)

-- | Just like 'V.unsafeUpd', except does a shallow copy when given an empty
-- list of updates.
vunsafeUpd :: V.Vector a -> [(Int, a)] -> V.Vector a
vunsafeUpd v [] = v
vunsafeUpd v xs = V.unsafeUpd v xs

-- | Just like 'U.unsafeUpd', except does a shallow copy when given an empty
-- list of updates.
uunsafeUpd :: U.Unbox a => U.Vector a -> [(Int, a)] -> U.Vector a
uunsafeUpd v [] = v
uunsafeUpd v xs = U.unsafeUpd v xs

-- | Overwrite the cells under a 'Pill', then repeatedly clear four-in-a-rows
-- and drop unsupported pieces. N.B. nothing will drop if nothing clears, so it
-- is the caller's responsibility to ensure that the pill would be supported
-- where it's being placed.
--
-- Returns 'Nothing' if the 'Pill' is out of bounds or over a non-'Empty' cell.
place :: Board -> Pill -> Maybe Board
place board pill = case (placementValid, fastPathValid) of
	(False, _) -> Nothing
	(_, True ) -> Just fastPath
	(_, False) -> Just slowPath
	where
	pos1@(Position x1 y1) = bottomLeftPosition pill
	pos2@(Position x2 y2) =      otherPosition pill

	-- There is no check on y2 here; either it's the same as y1, in which case
	-- the check on y1 suffices, or it's different, in which case it's above y1
	-- and allowed to be off the board anyway.
	placementValid :: Bool
	placementValid
		=  x1 >= 0 && x1 < width  board
		&&            x2 < width  board
		&& y1 >= 0 && y1 < height board
		&& unsafeGet board pos1 == Empty
		&& (y2 >= height board || unsafeGet board pos2 == Empty)

	-- We don't need to check y2 >= 0 because:
	-- * y2 >= y1
	-- * we only use y2Valid if case placementValid is True
	-- * if placementValid is True, then y1 >= 0
	y2Valid :: Bool
	y2Valid = y2 < height board

	fastPathValid :: Bool
	fastPathValid = and
		[ runLength pos dir got + runLength pos antidir got < 3
		| pos <- [pos1, pos2]
		, let got = getColor fastPath pos
		, (dir, antidir) <- [(left, right), (up, down)]
		]

	-- If we don't need to clear or drop anything, what would the board look
	-- like? We assume here that placementValid is True to elide some bounds
	-- checks.
	fastPath :: Board
	fastPath = case orientation pill of
		Horizontal -> board { cells = cells board `vunsafeUpd`
				[ (x, cells board `V.unsafeIndex` x `uunsafeUpd` [(y1, cell)])
				| (x, cell) <- [ (x1, Occupied (bottomLeftColor pill) West)
				               , (x2, Occupied (     otherColor pill) East)
				               ]
				]
			}
		Vertical -> board { cells = cells board `vunsafeUpd` [(x1, cells board `V.unsafeIndex` x1 `uunsafeUpd`
				(  [(y1, Occupied (bottomLeftColor pill) (if y2Valid then South else Disconnected))]
				++ [(y2, Occupied (     otherColor pill) North) | y2Valid]
				)
			)]}

	runLength :: Position -> Direction -> Maybe Color -> Int
	runLength pos dir Nothing = 0
	runLength pos dir got = go pos where
		go pos | getColor fastPath pos' == got = 1 + go pos'
		       | otherwise = 0
		       where pos' = unsafeMove dir pos

	slowPath = runST $ do
		mb <- thaw board
		ps <- case orientation pill of
			Horizontal -> do
				munsafeSet mb pos1 (Occupied (bottomLeftColor pill) West)
				munsafeSet mb pos2 (Occupied (     otherColor pill) East)
				return [pos1, pos2]
			Vertical | y2Valid -> do
				munsafeSet mb pos1 (Occupied (bottomLeftColor pill) South)
				munsafeSet mb pos2 (Occupied (     otherColor pill) North)
				return [pos1, pos2]
			_ -> do
				-- This is going to get cleared anyway, but making it
				-- Disconnected instead of South (so that one could merge with
				-- the previous case if desired) is good defensive programming.
				munsafeSet mb pos1 (Occupied (bottomLeftColor pill) Disconnected)
				return [pos2]
		ps <- unsafeClear mb ps
		unsafeDropAndClear mb ps
		munsafeFreeze mb

-- | Drop 'Disconnected' pieces, in the columns given by the keys and of the
-- colors given by the values. Returns 'Nothing' if any column is out of
-- bounds.
garbage :: Board -> Map Int Color -> Maybe Board
garbage board pieces | Map.size pieces <= 0 = return board -- uh...
garbage board pieces = do
	let (x , _) = Map.findMin pieces
	    (x', _) = Map.findMax pieces
	guard (x >= 0 && x' < width board)
	return $ runST $ do
		mb <- thaw board
		ps <- Map.traverseWithKey (go mb) pieces
		unsafeDropAndClear mb ps
		munsafeFreeze mb
	where
	go mb = let y = mheight mb - 1 in \x col -> do
		let p = Position x y
		munsafeSet mb p (Occupied col Disconnected)
		return p

data MBoard s = MBoard
	{ mwidth, mheight :: !Int
	, mcells :: !(M.MVector s Cell)
	}

memptyBoard
	:: Int -- ^ width
	-> Int -- ^ height
	-> ST s (MBoard s)
memptyBoard w h = MBoard w h <$> M.new (w*h)

thaw :: Board -> ST s (MBoard s)
thaw board = do
	-- the unsafeThaw should be safe because M.concat makes a new vector not
	-- used elsewhere
	cs <- U.unsafeThaw . U.concat . V.toList . cells $ board
	return MBoard
		{ mwidth  = width  board
		, mheight = height board
		, mcells  = cs
		}

-- | The 'MBoard' argument should not be used again after 'munsafeFreeze'ing it.
munsafeFreeze :: MBoard s -> ST s Board
munsafeFreeze MBoard { mwidth = w, mheight = h, mcells = mcs } = do
	cs <- U.unsafeFreeze mcs
	return Board
		{ height = h
		, cells  = V.generate w (\x -> U.slice (x * h) h cs)
		}

mfreeze :: MBoard s -> ST s Board
mfreeze MBoard { mwidth = w, mheight = h, mcells = mcs } = do
	cs <- U.freeze mcs
	return Board
		{ height = h
		, cells  = V.generate w (\x -> U.slice (x * h) h cs)
		}

-- | Doesn't do bounds checking.
munsafeGet :: MBoard s -> Position -> ST s Cell
munsafeGet mb p = M.unsafeRead (mcells mb) (x p * mheight mb + y p)

mget :: MBoard s -> Position -> ST s (Maybe Cell)
mget mb p
	| x p >= 0 && x p < mwidth mb && y p >= 0 && y p < mheight mb = Just <$> munsafeGet mb p
	| otherwise = return Nothing

-- | Doesn't do bounds checking, and doesn't promise anything about clears or
-- gravity.
munsafeSet :: MBoard s -> Position -> Cell -> ST s ()
munsafeSet mb p c = M.unsafeWrite (mcells mb) (x p * mheight mb + y p) c

-- | Modify the cell at a given position, and return the old value.
--
-- Doesn't do bounds checking or promise anything about clears or gravity.
munsafeModify :: MBoard s -> Position -> (Cell -> Cell) -> ST s Cell
munsafeModify mb p f = do
	c <- M.unsafeRead v i
	M.unsafeWrite v i (f c)
	return c
	where
	v = mcells mb
	i = x p * mheight mb + y p

-- | Place a virus. Out-of-bounds positions are silently discarded.
minfect :: MBoard s -> Position -> Color -> ST s ()
minfect mb p col
	| x p >= 0 && x p < mwidth mb && y p >= 0 && y p < mheight mb = munsafeSet mb p (Occupied col Virus)
	| otherwise = return ()

-- | @unsafeClear board positions@ takes a board and a collection of positions
-- on the board which have recently changed, and modifies the board to take
-- account of four-in-a-rows that involve the given positions. It returns a set
-- of positions which may need gravity applied to them.
--
-- It is the caller's responsibility to ensure that the given positions are in
-- bounds. Under that assumption, the returned positions definitely will be.
unsafeClear :: Foldable f => MBoard s -> f Position -> ST s (Set Position)
unsafeClear mb ps = do
	(clears_, disconnects_, drops_) <- unzip3 <$> mapM clearSingleMatch (toList ps)
	let clears = S.unions clears_
	    disconnects = S.unions disconnects_ `S.difference` clears
	    drops = (S.unions drops_ `S.difference` clears) `S.union` disconnects
	forM_ clears $ \p -> munsafeSet mb p Empty
	forM_ disconnects $ \p -> munsafeModify mb p disconnect
	return drops
	where
	disconnect (Occupied color shape) = Occupied color Disconnected
	-- do we want to check that the shape was not Virus or Disconnected before
	-- and throw an error then, too?
	disconnect Empty = error "The impossible happened: a pill was connected to an empty space."

	clearSingleMatch p = do
		[l,r,u,d] <- mapM (mrunLength mb p) [left, right, up, down]
		let clears = [p { x = x p + dx } | l+r+1 >= 4, dx <- [-l .. r]]
		          ++ [p { y = y p + dy } | u+d+1 >= 4, dy <- [-d .. u]]
		(disconnects_, drops_) <- unzip <$> mapM clearSingleCell clears
		return (S.fromList clears, S.unions disconnects_, S.unions drops_)

	clearSingleCell p = do
		old <- munsafeGet mb p
		let disconnects = case old of
		    	Occupied _ North -> ifInBounds down
		    	Occupied _ South -> drops -- ifInBounds up
		    	Occupied _ East  -> ifInBounds left
		    	Occupied _ West  -> ifInBounds right
		    	_ -> S.empty
		    drops = ifInBounds up
		    ifInBounds dir = let p'@(Position x y) = unsafeMove dir p in
		    	if x >= 0 && x < mwidth mb && y >= 0 && y < mheight mb
		    	then S.singleton p'
		    	else S.empty
		return (disconnects, drops)

-- | @unsafeDrop board positions@ takes a board and a collection of positions
-- on the board which may have recently become unsupported, and modifies the
-- board to take account of gravity for the given positions. It returns a set
-- of positions which may need to be involved in clears.
--
-- It is the caller's responsibility to ensure that the given positions are in
-- bounds. Under that assumption, the returned positions definitely will be.
unsafeDrop :: Foldable f => MBoard s -> f Position -> ST s (Set Position)
unsafeDrop mb ps = do
	ps_ <- mapM dropSingle (toList ps)
	let ps' = S.toAscList . S.unions $ ps_
	S.fromAscList <$> filterM isNotEmpty ps'
	where
	isNotEmpty p = (Empty/=) <$> munsafeGet mb p

	dropSingle p = do
		here <- munsafeGet mb p
		case here of
			Empty -> return S.empty
			Occupied _ Virus -> return S.empty
			Occupied _ East  -> dropDouble p (unsafeMove left  p) here
			Occupied _ West  -> dropDouble p (unsafeMove right p) here
			_ -> do
				dy <- mcolorRunLength mb p down Nothing
				if dy <= 0 then return S.empty else do
					let p'  = p { y = y p - dy }
					    y'' = y p + 1
					    p'' = p { y = y'' }
					munsafeSet mb p  Empty
					munsafeSet mb p' here
					if y'' >= mheight mb
						then return (S.singleton p')
						else S.insert p' <$> dropSingle p''

	dropDouble p p' here = do
		there <- munsafeGet mb p'
		dy_  <- mcolorRunLength mb p  down Nothing
		dy_' <- mcolorRunLength mb p' down Nothing
		let dy = min dy_ dy_'
		if dy <= 0 then return S.empty else do
			let pDown  = p  { y = y p  - dy }
			    pDown' = p' { y = y p' - dy }
			    yUp = y p + 1
			    pUp  = p  { y = yUp }
			    pUp' = p' { y = yUp }
			    drops = S.fromList [pDown, pDown']
			munsafeSet mb p  Empty
			munsafeSet mb p' Empty
			munsafeSet mb pDown   here
			munsafeSet mb pDown' there
			if yUp >= mheight mb
				then return drops
				else liftA2 (\ls rs -> S.unions [drops, ls, rs]) (dropSingle pUp) (dropSingle pUp')

-- | How far away is the last valid position with the same color as the given
-- position in the given direction?
mrunLength :: MBoard s -> Position -> Direction -> ST s Int
mrunLength mb p dir = do
	cell <- mget mb p
	case cell of
		Nothing -> return 0
		Just c -> runLengthHelper mb dir (color c) (unsafeMove dir p) 0

-- | How far away is the last valid position of the given color in the given
-- direction? (Does not check that the given position is of the given color.)
mcolorRunLength :: MBoard s -> Position -> Direction -> Maybe Color -> ST s Int
mcolorRunLength mb p dir col = runLengthHelper mb dir col (unsafeMove dir p) 0

-- | Internal use only.
runLengthHelper :: MBoard s -> Direction -> Maybe Color -> Position -> Int -> ST s Int
runLengthHelper mb dir = go where
	go col p n = do
		cell <- mget mb p
		case color <$> cell of
			Just col' | col' == col -> go col (unsafeMove dir p) $! n+1
			_ -> return n

-- | Loop applying gravity and clearing 4-in-a-rows, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeDropAndClear :: Foldable f => MBoard s -> f Position -> ST s ()
unsafeDropAndClear mb ps
	| null ps = return ()
	| otherwise = unsafeDrop mb ps >>= unsafeClearAndDrop mb

-- | Loop clearing 4-in-a-rows and applying gravity, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeClearAndDrop :: Foldable f => MBoard s -> f Position -> ST s ()
unsafeClearAndDrop mb ps
	| null ps = return ()
	| otherwise = unsafeClear mb ps >>= unsafeDropAndClear mb
