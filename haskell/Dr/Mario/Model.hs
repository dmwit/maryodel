{-# LANGUAGE ScopedTypeVariables #-}
module Dr.Mario.Model
	( Color(..)
	, Shape(..)
	, Cell(..), color, shape
	, Orientation(..)
	, Position(..)
	, Direction, left, right, down
	, Rotation(..)
	, PillContent(..), Pill(..), otherPosition
	, Board
	, emptyBoard
	, width, height
	, get, getColor, unsafeGet
	, move, rotate, place, garbage
	, pp
	, MBoard, IOBoard
	, thaw, mfreeze, munsafeFreeze
	, memptyBoard
	, mwidth, mheight
	, minfect, mplace
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified System.Console.ANSI         as ANSI

import Dr.Mario.Model.Internal

color :: Cell -> Maybe Color
color (Occupied color shape) = Just color
color Empty = Nothing

shape :: Cell -> Maybe Shape
shape (Occupied color shape) = Just shape
shape Empty = Nothing

data Orientation = Horizontal | Vertical deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | Uses the math convention: the bottom of a 'Board' is at 'y'=0, the top at some positive 'y'.
data Position = Position { x, y :: !Int } deriving (Eq, Ord, Read, Show)
data Direction = Direction { dx, dy :: !Int } deriving (Eq, Ord, Show)
data Rotation = Clockwise | Counterclockwise deriving (Bounded, Enum, Eq, Ord, Read, Show)
data PillContent = PillContent
	{ orientation :: !Orientation
	, bottomLeftColor, otherColor :: !Color
	} deriving (Eq, Ord, Read, Show)
data Pill = Pill
	{ content :: !PillContent
	, bottomLeftPosition :: !Position
	} deriving (Eq, Ord, Read, Show)

otherPosition :: Pill -> Position
otherPosition pill = unsafeMove dir pos where
	dir = case orientation (content pill) of
		Horizontal -> right
		Vertical   -> up
	pos = bottomLeftPosition pill

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
	then Just pill'
	else Nothing
	where
	pill' = pill { bottomLeftPosition = pos1 }
	pos1 = unsafeMove dir (bottomLeftPosition pill)
	pos2 = otherPosition pill'

-- | Rotate a pill, ignoring its location information or the context of the
-- board.
rotateContent :: PillContent -> Rotation -> PillContent
rotateContent content rot = case rot of
	Clockwise -> case or of
		Horizontal -> swapped
		Vertical   -> rotated
	Counterclockwise -> case or of
		Horizontal -> rotated
		Vertical   -> swapped
	where
	or = orientation content
	rotated = content { orientation = perpendicular or }
	swapped = rotated { bottomLeftColor = otherColor content, otherColor = bottomLeftColor content }

-- | Does no checks that the rotated pill would be in bounds, overlapping with
-- something on the board, etc. Just does it.
unsafeRotate :: Pill -> Rotation -> Pill
unsafeRotate pill rot = pill { content = rotateContent (content pill) rot }

-- | Rotate a pill in the given direction. 'Vertical' to 'Horizontal' rotations
-- kick left once if necessary. There are no vertical kicks. If there is no
-- room for the rotated version even after checking the kick position, returns
-- 'Nothing'.
rotate :: Board -> Pill -> Rotation -> Maybe Pill
rotate board pill rot = case orientation (content pill) of
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
	--
	-- Similarly we need not check that x1 is too large or that x2 is too
	-- small, because x1 <= x2.
	placementValid :: Bool
	placementValid
		=  x1 >= 0 && x2 < width  board
		&& y1 >= 0 && y1 < height board
		&& unsafeGet board pos1 == Empty
		&& (y2 >= height board || unsafeGet board pos2 == Empty)

	-- We don't need to check y2 >= 0 because:
	-- * we only use y2Valid if case placementValid is True
	-- * if placementValid is True, then y1 >= 0
	-- * y2 >= y1
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
	-- like?
	--
	-- We assume here that placementValid is True to elide some bounds checks.
	fastPath :: Board
	fastPath = case orientation (content pill) of
		Horizontal -> board { cells = cells board `V.unsafeUpd`
				[ (x, cells board `V.unsafeIndex` x `U.unsafeUpd` [(y1, cell)])
				| (x, cell) <- [ (x1, Occupied (bottomLeftColor (content pill)) West)
				               , (x2, Occupied (     otherColor (content pill)) East)
				               ]
				]
			}
		Vertical -> board { cells = cells board `V.unsafeUpd` [(x1, cells board `V.unsafeIndex` x1 `U.unsafeUpd`
				(  [(y1, Occupied (bottomLeftColor (content pill)) (if y2Valid then South else Disconnected))]
				++ [(y2, Occupied (     otherColor (content pill)) North) | y2Valid]
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
		munsafePlace mb pos1 pos2 (content pill)
		munsafeFreeze mb

-- | Drop 'Disconnected' pieces, in the columns given by the keys and of the
-- colors given by the values. Returns 'Nothing' if any column is out of
-- bounds.
garbage :: Board -> Map Int Color -> Maybe Board
garbage board pieces | M.size pieces <= 0 = return board -- uh...
garbage board pieces = do
	let (x , _) = M.findMin pieces
	    (x', _) = M.findMax pieces
	guard (x >= 0 && x' < width board)
	return $ runST $ do
		mb <- thaw board
		ps <- M.traverseWithKey (go mb) pieces
		unsafeDropAndClear mb ps
		munsafeFreeze mb
	where
	go mb = let y = mheight mb - 1 in \x col -> do
		let p = Position x y
		munsafeSet mb p (Occupied col Disconnected)
		return p

data MBoard s = MBoard
	{ mwidth, mheight :: !Int
	, mcells :: !(MV.MVector s Cell)
	}

-- | Just a convenient shorthand for the type of 'MBoard' that can be used in
-- 'IO'.
type IOBoard = MBoard (PrimState IO)

memptyBoard
	:: PrimMonad m
	=> Int -- ^ width
	-> Int -- ^ height
	-> m (MBoard (PrimState m))
memptyBoard w h = MBoard w h <$> MV.new (w*h)

thaw :: PrimMonad m => Board -> m (MBoard (PrimState m))
thaw board = do
	-- the unsafeThaw should be safe because U.concat makes a new vector not
	-- used elsewhere
	cs <- U.unsafeThaw . U.concat . V.toList . cells $ board
	return MBoard
		{ mwidth  = width  board
		, mheight = height board
		, mcells  = cs
		}

-- | The 'MBoard' argument should not be used again after 'munsafeFreeze'ing it.
munsafeFreeze :: PrimMonad m => MBoard (PrimState m) -> m Board
munsafeFreeze MBoard { mwidth = w, mheight = h, mcells = mcs } = do
	cs <- U.unsafeFreeze mcs
	return Board
		{ height = h
		, cells  = V.generate w (\x -> U.slice (x * h) h cs)
		}

mfreeze :: PrimMonad m => MBoard (PrimState m) -> m Board
mfreeze MBoard { mwidth = w, mheight = h, mcells = mcs } = do
	cs <- U.freeze mcs
	return Board
		{ height = h
		, cells  = V.generate w (\x -> U.slice (x * h) h cs)
		}

-- | Doesn't do bounds checking.
munsafeGet :: PrimMonad m => MBoard (PrimState m) -> Position -> m Cell
munsafeGet mb p = MV.unsafeRead (mcells mb) (x p * mheight mb + y p)

mget :: PrimMonad m => MBoard (PrimState m) -> Position -> m (Maybe Cell)
mget mb p
	| x p >= 0 && x p < mwidth mb && y p >= 0 && y p < mheight mb = Just <$> munsafeGet mb p
	| otherwise = return Nothing

-- | Doesn't do bounds checking, and doesn't promise anything about clears or
-- gravity.
munsafeSet :: PrimMonad m => MBoard (PrimState m) -> Position -> Cell -> m ()
munsafeSet mb p c = MV.unsafeWrite (mcells mb) (x p * mheight mb + y p) c

-- | Out-of-bounds writes are silently discarded. Doesn't promise anything
-- about clears or gravity.
mset :: PrimMonad m => MBoard (PrimState m) -> Position -> Cell -> m ()
mset mb p cell
	| x p >= 0 && x p < mwidth mb && y p >= 0 && y p < mheight mb = munsafeSet mb p cell
	| otherwise = return ()

-- | Modify the cell at a given position, and return the old value.
--
-- Doesn't do bounds checking or promise anything about clears or gravity.
munsafeModify :: PrimMonad m => MBoard (PrimState m) -> Position -> (Cell -> Cell) -> m Cell
munsafeModify mb p f = do
	c <- MV.unsafeRead v i
	MV.unsafeWrite v i (f c)
	return c
	where
	v = mcells mb
	i = x p * mheight mb + y p

-- | Place a virus. Out-of-bounds positions are silently discarded. Does not
-- trigger clears of 4-in-a-rows, so it is the caller's responsibility to
-- ensure this isn't needed.
minfect :: PrimMonad m => MBoard (PrimState m) -> Position -> Color -> m ()
minfect mb p col = mset mb p (Occupied col Virus)

-- | Overwrite the cells under a 'Pill', then repeatedly clear four-in-a-rows
-- and drop unsupported pieces. N.B. nothing will drop if nothing clears, so it
-- is the caller's responsibility to ensure that the pill would be supported
-- where it's being placed.
--
-- Does nothing if the 'Pill' is out of bounds or over a non-'Empty' cell.
mplace :: forall m. PrimMonad m => MBoard (PrimState m) -> Pill -> m ()
mplace mb pill = do
	valid <- placementValid
	when valid $ munsafePlace mb pos1 pos2 (content pill)
	where
	pos1@(Position x1 y1) = bottomLeftPosition pill
	pos2@(Position x2 y2) =      otherPosition pill

	-- We simplify the bounds checks with two assumptions: x1 <= x2, so we
	-- don't need to check if x1 is too big or x2 too small, and y2<=y1+1 and
	-- is allowed to be out of bounds by one, so it is a valid placement as
	-- long as y1 is in bounds.
	placementValid :: m Bool
	placementValid =
		       return (  x1 >= 0 && x2 < mwidth  mb
		              && y1 >= 0 && y1 < mheight mb
		              )
		`andM` fmap (Empty==) (munsafeGet mb pos1)
		`andM` if y2 < mheight mb
		       then fmap (Empty==) (munsafeGet mb pos2)
		       else return True

	infixr 3 `andM`
	andM :: m Bool -> m Bool -> m Bool
	andM l r = do
		v <- l
		if v then r else return False

-- | Doesn't check that the positions are sensible.
munsafePlace :: PrimMonad m => MBoard (PrimState m) -> Position -> Position -> PillContent -> m ()
munsafePlace mb pos1 pos2 pc = do
	ps <- case orientation pc of
		Horizontal -> do
			munsafeSet mb pos1 (Occupied (bottomLeftColor pc) West)
			munsafeSet mb pos2 (Occupied (     otherColor pc) East)
			return [pos1, pos2]
		Vertical | y pos2 < mheight mb -> do
			munsafeSet mb pos1 (Occupied (bottomLeftColor pc) South)
			munsafeSet mb pos2 (Occupied (     otherColor pc) North)
			return [pos1, pos2]
		_ -> do
			-- This is going to get cleared anyway, but making it
			-- Disconnected instead of South (so that one could merge with
			-- the previous case if desired) is good defensive programming.
			munsafeSet mb pos1 (Occupied (bottomLeftColor pc) Disconnected)
			return [pos2]
	ps <- unsafeClear mb ps
	unsafeDropAndClear mb ps

-- | @unsafeClear board positions@ takes a board and a collection of positions
-- on the board which have recently changed, and modifies the board to take
-- account of four-in-a-rows that involve the given positions. It returns a set
-- of positions which may need gravity applied to them.
--
-- It is the caller's responsibility to ensure that the given positions are in
-- bounds. Under that assumption, the returned positions definitely will be.
unsafeClear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m (Set Position)
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
unsafeDrop :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m (Set Position)
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
mrunLength :: PrimMonad m => MBoard (PrimState m) -> Position -> Direction -> m Int
mrunLength mb p dir = do
	cell <- mget mb p
	case cell of
		Nothing -> return 0
		Just c -> mcolorRunLength mb p dir (color c)

-- | How far away is the last valid position of the given color in the given
-- direction? (Does not check that the given position is of the given color.)
mcolorRunLength :: PrimMonad m => MBoard (PrimState m) -> Position -> Direction -> Maybe Color -> m Int
mcolorRunLength mb p dir col = go (unsafeMove dir p) 0 where
	go p n = do
		cell <- mget mb p
		case color <$> cell of
			Just col' | col' == col -> go (unsafeMove dir p) $! n+1
			_ -> return n

-- | Loop applying gravity and clearing 4-in-a-rows, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeDropAndClear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m ()
unsafeDropAndClear mb ps
	| null ps = return ()
	| otherwise = unsafeDrop mb ps >>= unsafeClearAndDrop mb

-- | Loop clearing 4-in-a-rows and applying gravity, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeClearAndDrop :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m ()
unsafeClearAndDrop mb ps
	| null ps = return ()
	| otherwise = unsafeClear mb ps >>= unsafeDropAndClear mb
