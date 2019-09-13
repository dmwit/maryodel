{-# LANGUAGE ScopedTypeVariables #-}
module Dr.Mario.Model
	( Color(..)
	, Shape(..)
	, Cell(..), color, shape
	, Orientation(..), bottomLeftShape, otherShape
	, Position(..)
	, Direction, left, right, down
	, Rotation(..)
	, PillContent(..), bottomLeftCell, otherCell
	, Pill(..), otherPosition
	, Board
	, emptyBoard
	, width, height
	, get, getColor, unsafeGet
	, move, rotate, rotateContent, place, garbage, clear
	, randomBoard, unsafeRandomViruses
	, advanceRNG, decodeColor, decodePosition
	, startingBottomLeftPosition, startingOtherPosition, startingOrientation, launchPill
	, pp
	, MBoard, IOBoard
	, thaw, mfreeze, munsafeFreeze
	, memptyBoard
	, mwidth, mheight
	, mget, munsafeGet
	, minfect, mplace, mgarbage, mclear
	, mrandomBoard, munsafeRandomViruses
	, mnewRNG, mrandomColor, mrandomPosition
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Writer.CPS
import Data.Bits hiding (rotate)
import Data.Foldable (toList, for_)
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Map (Map)
import Data.Monoid
import Data.Primitive.MutVar
import Data.Set (Set)
import Data.Word
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified System.Console.ANSI         as ANSI

import Dr.Mario.Model.Internal
import Dr.Mario.Util

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

instance Hashable Orientation where hashWithSalt = hashUsing fromEnum

instance Hashable Position where
	hashWithSalt s pos = s
		`hashWithSalt` x pos
		`hashWithSalt` y pos

instance Hashable PillContent where
	hashWithSalt s pc = s
		`hashWithSalt` orientation pc
		`hashWithSalt` bottomLeftColor pc
		`hashWithSalt` otherColor pc

instance Hashable Pill where
	hashWithSalt s pill = s
		`hashWithSalt` content pill
		`hashWithSalt` bottomLeftPosition pill

bottomLeftShape :: Orientation -> Shape
bottomLeftShape Horizontal = West
bottomLeftShape Vertical = South

otherShape :: Orientation -> Shape
otherShape Horizontal = East
otherShape Vertical = North

bottomLeftCell :: PillContent -> Cell
bottomLeftCell c = Occupied (bottomLeftColor c) (bottomLeftShape (orientation c))

otherCell :: PillContent -> Cell
otherCell c = Occupied (otherColor c) (otherShape (orientation c))

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
-- Otherwise returns the new 'Board' and the number of viruses that were
-- cleared.
place :: Board -> Pill -> Maybe (Int, Board)
place board pill = case (placementValid, fastPathValid) of
	(False, _) -> Nothing
	(_, True ) -> Just (0, fastPath)
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
		liftA2 (,)
			(munsafePlace mb pos1 pos2 (content pill))
			(munsafeFreeze mb)

-- | Drop 'Disconnected' pieces, in the columns given by the keys and of the
-- colors given by the values. Returns 'Nothing' if any column is out of
-- bounds.
garbage :: Board -> Map Int Color -> Maybe Board
garbage board pieces | M.size pieces <= 0 = return board -- uh...
garbage board pieces = do
	guard (unsafeGarbageInBounds (width board) pieces)
	return $ runST $ do
		mb <- thaw board
		munsafeGarbage mb pieces
		munsafeFreeze mb

-- | Set the given positions to 'Empty', then apply gravity and clear
-- four-in-a-rows.
clear :: Foldable f => Board -> f Position -> Board
clear board ps = runST $ do
	mb <- thaw board
	mclear mb ps
	munsafeFreeze mb

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
-- Returns 'Nothing' (and does nothing else) if the 'Pill' is out of bounds or
-- over a non-'Empty' cell; otherwise returns the number of viruses cleared.
mplace :: forall m. PrimMonad m => MBoard (PrimState m) -> Pill -> m (Maybe Int)
mplace mb pill = do
	valid <- placementValid
	if valid
		then Just <$> munsafePlace mb pos1 pos2 (content pill)
		else return Nothing
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

-- | Doesn't check that the positions are sensible. Returns the number of viruses cleared.
munsafePlace :: PrimMonad m => MBoard (PrimState m) -> Position -> Position -> PillContent -> m Int
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
			munsafeSet mb pos1 (Occupied (bottomLeftColor pc) Disconnected)
			return [pos1]
	getSum <$> execWriterT (unsafeClearAndDrop mb ps)

-- | Drop 'Disconnected' pieces, in the columns given by the keys and of the
-- colors given by the values. Returns 'False' (without changing the board) if
-- any column is out of bounds, 'True' otherwise.
mgarbage :: PrimMonad m => MBoard (PrimState m) -> Map Int Color -> m Bool
mgarbage mb pieces | M.size pieces <= 0 = return True -- uh...
mgarbage mb pieces = if unsafeGarbageInBounds (mwidth mb) pieces
	then True <$ munsafeGarbage mb pieces
	else return False

-- | Check if the given collection of columns is between 0 and the width given
-- by the first argument. Assumes that the collection of columns is nonempty.
unsafeGarbageInBounds :: Int -> Map Int Color -> Bool
unsafeGarbageInBounds w pieces = x >= 0 && x' < w where
	(x , _) = M.findMin pieces
	(x', _) = M.findMax pieces

-- | Doesn't check that the columns are in-bounds.
munsafeGarbage :: PrimMonad m => MBoard (PrimState m) -> Map Int Color -> m ()
munsafeGarbage mb pieces = do
	ps <- M.traverseWithKey go pieces
	fst <$> runWriterT (unsafeDropAndClear mb ps)
	where
	y = mheight mb - 1
	go x col = p <$ munsafeSet mb p (Occupied col Disconnected) where
		p = Position x y

-- | Set the given positions to 'Empty', then apply gravity and clear
-- four-in-a-rows.
mclear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m ()
mclear mb ps = do
	for_ ps $ \p -> mset mb p Empty
	fmap fst . runWriterT $ unsafeDropAndClear mb
		[ p { y = y' }
		| p <- toList ps
		, let y' = y p + 1
		, x p >= 0 && x p < mwidth mb && y p >= 0 && y' < mheight mb
		]

unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 [] = ([],[],[],[])
unzip4 ((a,b,c,d):abcds) = let (as,bs,cs,ds) = unzip4 abcds in (a:as,b:bs,c:cs,d:ds)

-- | @unsafeClear board positions@ takes a board and a collection of positions
-- on the board which have recently changed, and modifies the board to take
-- account of four-in-a-rows that involve the given positions. It returns a set
-- of positions which may need gravity applied to them.
--
-- It is the caller's responsibility to ensure that the given positions are in
-- bounds. Under that assumption, the returned positions definitely will be.
unsafeClear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> WriterT (Sum Int) m (Set Position)
unsafeClear mb ps = do
	(clears_, disconnects_, drops_, viruses_) <- unzip4 <$> mapM clearSingleMatch (toList ps)
	let clears = S.unions clears_
	    disconnects = S.unions disconnects_ `S.difference` clears
	    drops = (S.unions drops_ `S.difference` clears) `S.union` disconnects
	    viruses = S.unions viruses_
	forM_ clears $ \p -> munsafeSet mb p Empty
	forM_ disconnects $ \p -> munsafeModify mb p disconnect
	tell (Sum (S.size viruses))
	return drops
	where
	disconnect (Occupied color shape) = Occupied color Disconnected
	-- do we want to check that the shape was not Virus or Disconnected before
	-- and throw an error then, too?
	disconnect Empty = error "The impossible happened: a pill was connected to an empty space."

	clearSingleMatch p = do
		-- the ~ avoids an annoying MonadFail constraint, and should be safe in
		-- this case
		~[l,r,u,d] <- mapM (mrunLength mb p) [left, right, up, down]
		let clears = [p { x = x p + dx } | l+r+1 >= 4, dx <- [-l .. r]]
		          ++ [p { y = y p + dy } | u+d+1 >= 4, dy <- [-d .. u]]
		(disconnects_, drops_, viruses_) <- unzip3 <$> mapM clearSingleCell clears
		return (S.fromList clears, S.unions disconnects_, S.unions drops_, S.unions viruses_)

	clearSingleCell p = do
		old <- munsafeGet mb p
		let disconnects = case old of
		    	Occupied _ North -> ifInBounds down
		    	Occupied _ South -> drops -- ifInBounds up
		    	Occupied _ East  -> ifInBounds left
		    	Occupied _ West  -> ifInBounds right
		    	_ -> S.empty
		    viruses = case old of
		    	Occupied _ Virus -> S.singleton p
		    	_ -> S.empty
		    drops = ifInBounds up
		    ifInBounds dir = let p'@(Position x y) = unsafeMove dir p in
		    	if x >= 0 && x < mwidth mb && y >= 0 && y < mheight mb
		    	then S.singleton p'
		    	else S.empty
		return (disconnects, drops, viruses)

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
unsafeDropAndClear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> WriterT (Sum Int) m ()
unsafeDropAndClear mb ps
	| null ps = return ()
	| otherwise = unsafeDrop mb ps >>= unsafeClearAndDrop mb

-- | Loop clearing 4-in-a-rows and applying gravity, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeClearAndDrop :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> WriterT (Sum Int) m ()
unsafeClearAndDrop mb ps
	| null ps = return ()
	| otherwise = unsafeClear mb ps >>= unsafeDropAndClear mb

-- | An implementation of the random number generator. Given a current RNG
-- state, produces the next one.
advanceRNG :: Word16 -> Word16
advanceRNG seed = seed `shiftR` 1 .|. if testBit seed 1 == testBit seed 9 then 0 else bit 15

-- | Choose a color from an RNG state in the same (biased) way that Dr. Mario does.
decodeColor :: Word16 -> Color
-- TODO: unpack this vector, maybe
decodeColor = \seed -> colorTable V.! fromIntegral (seed .&. 0xf) where
	colorTable = V.fromListN 16 [Yellow,Red,Blue,Blue,Red,Yellow,Yellow,Red,Blue,Blue,Red,Yellow,Yellow,Red,Blue,Red]

-- | Choose a position from an RNG state in the same way that Dr. Mario does
-- (hence specifically for boards of width 8 and height 16).
decodePosition :: Word16 -> Position
decodePosition seed = Position
	{ x = fromIntegral (seed .&. 0x0007)
	, y = fromIntegral ((seed .&. 0x0f00) `shiftR` 8)
	}

-- | The position that Dr. Mario initially launches a pill at on the NES.
startingBottomLeftPosition :: Position
startingBottomLeftPosition = Position 3 15

-- | The position of the other half of a pill when Dr. Mario initially launches
-- it on the NES.
startingOtherPosition :: Position
startingOtherPosition = Position 4 15

-- | The orientation that Dr. Mario initially launches a pill in on the NES.
startingOrientation :: Orientation
startingOrientation = Horizontal

-- | Initialize a pill in the position and orientation that Dr. Mario launches
-- them in on the NES.
launchPill :: Color -> Color -> Pill
launchPill l r = Pill (PillContent startingOrientation l r) startingBottomLeftPosition

-- | A call to @unsafeRandomViruses w h n mpos mcol@ will generate a random
-- board of width @w@, height @h@, and with @n@ viruses, using the same
-- virus-placement logic that Dr. Mario uses, but with your custom logic for
-- choosing a position (@mpos@) and color (@mcol@). See 'mnewRNG',
-- 'mrandomColor', and 'mrandomPosition' if you would like to use the same
-- logic as Dr. Mario itself for picking positions and colors.
--
-- It is unsafe because it may loop forever if @n@ is big enough that it gets
-- stuck trying to find a suitable virus location.
unsafeRandomViruses :: PrimMonad m => Int -> Int -> Int -> m Position -> m Color -> m Board
unsafeRandomViruses w h n mpos mcol = do
	mb <- memptyBoard w h
	munsafeRandomViruses mb n mpos mcol
	munsafeFreeze mb

-- | @randomBoard seed level@ generates a random starting board in exactly the
-- same way that Dr. Mario would, starting from RNG state given by @seed@, and
-- for virus level @level@ (usually in the range 0-24).
randomBoard :: Word16 -> Int -> Board
randomBoard seed level = runST (mrandomBoard seed level >>= munsafeFreeze)

-- | Given a seed, produce an action which you can call repeatedly to get the
-- random sequence that the NES Dr. Mario's random number generator would
-- produce. The seed is not produced as the first result (unless the seed is
-- 0).
mnewRNG :: PrimMonad m => Word16 -> m (m Word16)
mnewRNG seed = do
	ref <- newMutVar seed
	return $ do
		-- Could modifyMutVar >> readMutVar, but that's one more dereference
		-- than this. I wish modifyMutVar would just return the new result...
		seed' <- advanceRNG <$> readMutVar ref
		writeMutVar ref seed'
		return seed'

-- | Turn a random number generator action into an action that produces a
-- random color in the same (biased) way that Dr. Mario does.
mrandomColor :: Functor m => m Word16 -> m Color
mrandomColor = fmap decodeColor

-- | Turn a random number generator action into an action that produces a
-- random position at a given maximum height in the same way that Dr. Mario
-- does (i.e. using rejection sampling).
mrandomPosition :: Monad m => Int -> m Word16 -> m Position
mrandomPosition height mrng = go where
	go = do
		pos <- decodePosition <$> mrng
		if y pos > height then go else return pos

-- TODO: Some ideas for performance improvements:
-- 1. Keep a data structure that maps positions to virus colors that can go
--    there.
-- 2. Keep a data structure that, for each position, remembers what position we
--    ended up skipping to last time we tried to place a virus there.

-- | Given a virus position and color, use the same routine Dr. Mario does to
-- try to place a virus of a similar color near that position. Reports whether
-- it succeeded or gave up.
mtryPlaceVirus :: PrimMonad m => MBoard (PrimState m) -> Position -> Color -> m Bool
mtryPlaceVirus mb pos_ c_ = go (clipPosition pos_) where
	w = mwidth  mb
	h = mheight mb

	clipPosition pos = Position
		{ x = max 0 . min (w-1) $ x pos
		, y = max 0 . min (h-1) $ y pos
		}

	virusColors mcells = [color | Just (Occupied color Virus) <- mcells]
	neighborColors pos = do
		here <- munsafeGet mb pos
		case here of
			Empty -> virusColors <$> traverse (mget mb)
				[ pos { x = x pos - 2 }
				, pos { x = x pos + 2 }
				, pos { y = y pos - 2 }
				, pos { y = y pos + 2 }
				]
			_ -> return [Red,Yellow,Blue]

	colorsToTry = take 3 . dropWhile (c_/=) $ cycle [Blue,Red,Yellow]

	advancePosition pos
		| x' >= w = Position { y = y pos - 1, x = 0 }
		| otherwise = pos { x = x' }
		where x' = x pos + 1

	go pos
		| y pos < 0 = return False
		| otherwise = do
			invalidColors <- neighborColors pos
			case colorsToTry L.\\ invalidColors of
				[] -> go (advancePosition pos)
				c:_ -> True <$ minfect mb pos c

-- | Place a single virus using the same logic that Dr. Mario uses. This is
-- unsafe because it may loop forever if no suitable location can be found.
munsafeRandomVirus :: PrimMonad m => MBoard (PrimState m) -> m Position -> m Color -> m ()
munsafeRandomVirus mb mpos mc = go where
	go = do
		pos <- mpos
		c <- mc
		success <- mtryPlaceVirus mb pos c
		unless success go

-- | Populate a board with viruses using the same logic that Dr. Mario uses.
-- This is unsafe because it may loop forever if it can't find enough suitable
-- virus locations. The 'Int' argument is how many viruses to place.
munsafeRandomViruses ::
	PrimMonad m =>
	MBoard (PrimState m) ->
	Int ->
	m Position ->
	m Color ->
	m ()
munsafeRandomViruses mb virusCount mpos mc
	= for_ colorGenerators (munsafeRandomVirus mb mpos)
	where
	-- Technically, this should start a different place in the list depending
	-- on virusCount .&. 3. But the original game always chooses a virus count
	-- that is a multiple of 4, so that behavior can never be observed;
	-- therefore we simplify.
	colorGenerators = take virusCount
		$ cycle [return Yellow, mc, return Blue, return Red]

-- mrandomBoard is not unsafe, because I have exhaustively tested that for each
-- seed, we successfully generate a board without looping for levels 14, 16,
-- 18, and 20 (the maximum virus counts for each possible maximum height).

-- | Given a seed and a virus level (usually in the range 0-24), generate a
-- random board of the standard size in the same way Dr. Mario does.
mrandomBoard :: PrimMonad m => Word16 -> Int -> m (MBoard (PrimState m))
mrandomBoard seed level = do
	mb <- memptyBoard 8 16
	mrng <- mnewRNG seed
	munsafeRandomViruses mb virusCount (mrandomPosition height mrng) (mrandomColor mrng)
	return mb
	where
	height     = max 9 . min 12 $ (level+5) `shiftR` 1
	virusCount = max 4 . min 84 $ (level+1) `shiftL` 2
