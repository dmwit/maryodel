{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dr.Mario.Model
	( Color(..)
	, Shape(..)
	, Cell(..), toOCell, color, shape
	, OCell(..), toCell
	, Orientation(..), bottomLeftShape, otherShape, perpendicular
	, Position(..)
	, Direction, left, right, down, unsafeMove
	, Rotation(..), chiral
	, Lookahead(..), lookaheadFromPillContent, lookaheadFromPill, mirror
	, PillContent(..), pillContentFromLookahead, bottomLeftCell, otherCell
	, Pill(..), otherPosition
	, CoarseSpeed(..), gravityTable, gravityIndex, gravity
	, CleanupResults(..)
	, DropResults(..), summarizeDropResults
	, ClearResults(..), summarizeClearResults
	, Board
	, emptyBoard, unsafeGenerateBoard
	, width, height
	, get, getColor, unsafeGet, ofoldMap, ofoldMapWithKey, unsafeMap, countViruses
	, move, rotate, rotateContent, place, placeDetails, garbage, clear
	, randomLevel, randomBoard, unsafeRandomViruses, randomLookaheads
	, advanceRNG, retreatRNG, retreatRNG', decodeColor, decodePosition, lookaheadTable
	, startingBottomLeftPosition, startingOtherPosition, startingOrientation, launchPill, launchContent
	, ntscFrameRate
	, pp, ppIO, mppIO, mppST
	, MBoard, IOBoard
	, thaw, mfreeze, munsafeFreeze
	, memptyBoard
	, mwidth, mheight
	, mget, munsafeGet, mofoldMap, mofoldMapWithKey, mcountViruses
	, minfect, mplace, mplaceDetails, mgarbage, mclear
	, mrandomLevel, munsafeRandomLevel, mrandomBoard, munsafeRandomBoard, munsafeRandomViruses, mrandomLookaheads
	, mnewRNG, mrandomColor, mrandomPosition
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Writer.CPS
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.Bits hiding (rotate)
import Data.Foldable (toList, foldMap', for_)
import Data.Functor.Contravariant
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Ix
import Data.Map (Map)
import Data.Semigroup
import Data.Monoid
import Data.Primitive.MutVar
import Data.Set (Set)
import Data.Word
import GHC.Generics
import System.IO
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified System.Console.ANSI         as ANSI

import Dr.Mario.Model.Internal
import Dr.Mario.Util

-- | Uses the math convention: the bottom of a 'Board' is at 'y'=0, the top at some positive 'y'.
data Position = Position { x, y :: !Int } deriving (Eq, Ord, Read, Show)
data Direction = Direction { dx, dy :: !Int } deriving (Eq, Ord, Show)
data Lookahead = Lookahead { leftColor, rightColor :: !Color } deriving (Eq, Ord, Read, Show)
data PillContent = PillContent
	{ orientation :: !Orientation
	, bottomLeftColor, otherColor :: !Color
	} deriving (Eq, Ord, Read, Show)
data Pill = Pill
	{ content :: !PillContent
	, bottomLeftPosition :: !Position
	} deriving (Eq, Ord, Read, Show)
data CoarseSpeed = Low | Med | Hi | Ult deriving (Bounded, Enum, Eq, Ord, Read, Show, Generic)

-- | The O is for occupied.
data OCell = OCell
	{ ocolor :: !Color
	, oshape :: !Shape
	} deriving (Eq, Ord, Read, Show)

instance Hashable Position where
	hashWithSalt s pos = s
		`hashWithSalt` x pos
		`hashWithSalt` y pos

instance ToJSON Position where toJSON pos = toJSON (x pos, y pos)
instance FromJSON Position where parseJSON v = uncurry Position <$> parseJSON v
instance ToJSONKey Position
instance FromJSONKey Position

instance Hashable Direction where
	hashWithSalt s Direction { dx = x, dy = y } = s
		`hashWithSalt` x
		`hashWithSalt` y

instance Hashable Lookahead where
	hashWithSalt s lk = s
		`hashWithSalt` leftColor lk
		`hashWithSalt` rightColor lk

ppLookahead :: Lookahead -> String
ppLookahead lk = toChar <$> [leftColor lk, rightColor lk]

-- No toJSONList/toJSONKeyList implementation; same reason as for PillContent.
instance ToJSON    Lookahead where toJSON = toJSON . ppLookahead
instance ToJSONKey Lookahead where toJSONKey = contramap ppLookahead toJSONKey

parseLookahead :: String -> Parser Lookahead
parseLookahead s = case s of
	[l, r] -> pure Lookahead
		<*> parseSingleCharOr err l
		<*> parseSingleCharOr err r
	_ -> err
	where
	err :: Parser a
	err = fail $ "expected Lookahead, which is a string with two color characters, but instead saw " ++ show s

instance FromJSON    Lookahead where parseJSON = parseJSON >=> parseLookahead
instance FromJSONKey Lookahead where fromJSONKey = FromJSONKeyTextParser (parseLookahead . T.unpack)

instance Hashable PillContent where
	hashWithSalt s pc = s
		`hashWithSalt` orientation pc
		`hashWithSalt` bottomLeftColor pc
		`hashWithSalt` otherColor pc

ppPillContent :: PillContent -> String
ppPillContent pc =
	[ toChar (orientation pc)
	, toChar (bottomLeftColor pc)
	, toChar (otherColor pc)
	]

-- No toJSONList/toJSONKeyList implementation. Although smashing together the
-- strings for each PillContent would result in something that was
-- unambiguously parseable back into a list of PillContents, I just like the
-- list-y syntax better than the mashed-string-y syntax for this type.
instance ToJSON    PillContent where toJSON = toJSON . ppPillContent
instance ToJSONKey PillContent where toJSONKey = contramap ppPillContent toJSONKey

parsePillContent :: String -> Parser PillContent
parsePillContent s = case s of
	[orient, bl, other] -> pure PillContent
		<*> parseSingleCharOr err orient
		<*> parseSingleCharOr err bl
		<*> parseSingleCharOr err other
	_ -> err
	where
	err :: Parser a
	err = fail $ "expected PillContent, which is a string with an orientation character followed by two color characters, but instead saw " ++ show s

instance FromJSON    PillContent where parseJSON = parseJSON >=> parsePillContent
instance FromJSONKey PillContent where fromJSONKey = FromJSONKeyTextParser (parsePillContent . T.unpack)

instance Hashable Pill where
	hashWithSalt s pill = s
		`hashWithSalt` content pill
		`hashWithSalt` bottomLeftPosition pill

instance ToJSON Pill where toJSON pill = toJSON (content pill, bottomLeftPosition pill)
instance FromJSON Pill where parseJSON v = uncurry Pill <$> parseJSON v
instance ToJSONKey Pill
instance FromJSONKey Pill

instance ToJSON CoarseSpeed
instance FromJSON CoarseSpeed

instance Hashable OCell where
	hashWithSalt s oc = s
		`hashWithSalt` ocolor oc
		`hashWithSalt` oshape oc

ocellShowS :: OCell -> String -> String
ocellShowS (OCell c s) rest = toChar c : toChar s : rest

parseOCell :: (forall a. Parser a) -> Char -> Char -> Parser OCell
parseOCell err c s = pure OCell <*> parseSingleCharOr err c <*> parseSingleCharOr err s

ocellFromSource :: ParserSource src => src -> Parser OCell
ocellFromSource src = srcString src >>= \case
	[c, s] -> parseOCell err c s
	_ -> err
	where
	err :: Parser a
	err = mismatch "occupied Cell (color character followed by shape character)" src

ocellsFromSource :: ParserSource src => src -> Parser [OCell]
ocellsFromSource src = srcString src >>= go where
	go (c:s:rest) = pure (:) <*> parseOCell err c s <*> go rest
	go [] = pure []
	go _ = err

	err :: Parser a
	err = mismatch "[OCell] (string of alternating colors and shapes)" src

instance ToJSON OCell where
	toJSON = toJSON . flip ocellShowS ""
	toEncoding = toEncoding . flip ocellShowS ""
	toJSONList = toJSON . foldr ocellShowS ""
	toEncodingList = toEncoding . foldr ocellShowS ""

instance ToJSONKey OCell where
	toJSONKey = contramap (flip ocellShowS "") toJSONKey
	toJSONKeyList = contramap (foldr ocellShowS "") toJSONKey

instance FromJSON OCell where
	parseJSON = ocellFromSource
	parseJSONList = ocellsFromSource

instance FromJSONKey OCell where
	fromJSONKey = FromJSONKeyTextParser ocellFromSource
	fromJSONKeyList = FromJSONKeyTextParser ocellsFromSource

toOCell :: Cell -> Maybe OCell
toOCell Empty = Nothing
toOCell (Occupied c s) = Just (OCell c s)

toCell :: OCell -> Cell
toCell (OCell c s) = Occupied c s

color :: Cell -> Maybe Color
color = fmap ocolor . toOCell

shape :: Cell -> Maybe Shape
shape = fmap oshape . toOCell

bottomLeftShape :: Orientation -> Shape
bottomLeftShape Horizontal = West
bottomLeftShape Vertical = South

otherShape :: Orientation -> Shape
otherShape Horizontal = East
otherShape Vertical = North

mirror :: Lookahead -> Lookahead
mirror (Lookahead l r) = Lookahead r l

pillContentFromLookahead :: Orientation -> Lookahead -> PillContent
pillContentFromLookahead o lk = PillContent
	{ orientation = o
	, bottomLeftColor = leftColor lk
	, otherColor = rightColor lk
	}

lookaheadFromPillContent :: PillContent -> Lookahead
lookaheadFromPillContent pc = Lookahead (bottomLeftColor pc) (otherColor pc)

lookaheadFromPill :: Pill -> Lookahead
lookaheadFromPill = lookaheadFromPillContent . content

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

chiral :: Rotation -> Rotation
chiral Clockwise = Counterclockwise
chiral Counterclockwise = Clockwise

left, right, up, down :: Direction
left  = Direction (-1)  0
right = Direction   1   0
up    = Direction   0   1
down  = Direction   0 (-1)

-- | For NTSC NES Dr. Mario.
gravityTable :: U.Vector Int
gravityTable = U.fromList [70, 68, 66, 64, 62, 60, 58, 56, 54, 52, 50, 48, 46, 44, 42, 40, 38, 36, 34, 32, 30, 28, 26, 24, 22, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 10, 9, 9, 8, 8, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1]

-- | The starting index into 'gravityTable' used by NES Dr. Mario.
gravityIndex :: CoarseSpeed -> Int
gravityIndex = \case
	Low -> 15
	Med -> 25
	Hi  -> 31
	Ult -> 35

-- | Compute the gravity (in frames per row) the same way NES Dr. Mario does.
-- The @Int@ argument is how many pills have already been locked since the
-- level started. Probably doesn't do anything sane with negative inputs, but
-- at least it won't crash.
gravity :: CoarseSpeed -> Int -> Int
gravity speed pills = gravityTable `U.unsafeIndex` (max 0 . min 80) (gravityIndex speed + pillPenalty) where
	pillPenalty = min 49 $ (pills+2) `quot` 10

width :: Board -> Int
width = V.length . cells

-- | Unsafe because it does not perform clears or gravity if those are needed.
unsafeGenerateBoard
	:: Int -- ^ width
	-> Int -- ^ height
	-> (Position -> Cell) -- ^ board contents
	-> Board
unsafeGenerateBoard w h f = Board h (V.generate w (\x -> U.generate h (f . Position x)))

get :: Board -> Position -> Maybe Cell
get b p = (V.!? x p) >=> (U.!? y p) $ cells b

getColor :: Board -> Position -> Maybe Color
getColor b = get b >=> color

-- | Does not check that the position is in bounds.
unsafeGet :: Board -> Position -> Cell
unsafeGet b p = cells b `V.unsafeIndex` x p `U.unsafeIndex` y p

-- | The frame rate the NTSC NES outputs (in frames per second).
ntscFrameRate :: Fractional a => a
ntscFrameRate = 60.0988

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

-- the flush is needed because the color reset is after the final newline, and
-- the caller might be about to let the user type on stdin with echoing on
ppIO :: Board -> IO ()
ppIO b = putStr (pp b) >> hFlush stdout

mppIO :: IOBoard -> IO ()
mppIO = mfreeze >=> ppIO

mppST :: PrimMonad m => MBoard (PrimState m) -> m String
mppST = fmap pp . mfreeze

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

-- TODO: make a specialized version that does less allocation of intermediate
-- data structures
-- | Like 'placeDetails', but with less detail about what all happened.
place :: Board -> Pill -> Maybe (CleanupResults, Board)
place board pill = first summarizeClearResults <$> placeDetails board pill

-- | Overwrite the cells under a 'Pill', then repeatedly clear four-in-a-rows
-- and drop unsupported pieces. N.B. nothing will drop if nothing clears, so it
-- is the caller's responsibility to ensure that the pill would be supported
-- where it's being placed.
--
-- Returns 'Nothing' if the 'Pill' is out of bounds or over a non-'Empty' cell.
-- Otherwise returns the new 'Board' and a summary of what all happened.
placeDetails :: Board -> Pill -> Maybe (ClearResults, Board)
placeDetails board pill = case (placementValid, fastPathValid) of
	(False, _) -> Nothing
	(_, True ) -> Just (NoClear, fastPath)
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
	-- * we only use y2Valid if placementValid is True
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

-- | Just a convenient shorthand for the type of 'MBoard' that can be used in
-- 'IO'.
type IOBoard = MBoard (PrimState IO)

memptyBoard
	:: PrimMonad m
	=> Int -- ^ width
	-> Int -- ^ height
	-> m (MBoard (PrimState m))
memptyBoard w h = MBoard w h <$> MU.new (w*h)

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
munsafeGet mb p = MU.unsafeRead (mcells mb) (x p * mheight mb + y p)

mget :: PrimMonad m => MBoard (PrimState m) -> Position -> m (Maybe Cell)
mget mb p
	| x p >= 0 && x p < mwidth mb && y p >= 0 && y p < mheight mb = Just <$> munsafeGet mb p
	| otherwise = return Nothing

-- | Doesn't do bounds checking, and doesn't promise anything about clears or
-- gravity.
munsafeSet :: PrimMonad m => MBoard (PrimState m) -> Position -> Cell -> m ()
munsafeSet mb p c = MU.unsafeWrite (mcells mb) (x p * mheight mb + y p) c

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
	c <- MU.unsafeRead v i
	MU.unsafeWrite v i (f c)
	return c
	where
	v = mcells mb
	i = x p * mheight mb + y p

-- | Place a virus. Out-of-bounds positions are silently discarded. Does not
-- trigger clears of 4-in-a-rows, so it is the caller's responsibility to
-- ensure this isn't needed.
minfect :: PrimMonad m => MBoard (PrimState m) -> Position -> Color -> m ()
minfect mb p col = mset mb p (Occupied col Virus)

-- TODO: make a specialized version that does less allocation of intermediate
-- data structures
-- | Like 'mplaceDetails', but with less detail about what all happened.
mplace :: forall m. PrimMonad m => MBoard (PrimState m) -> Pill -> m (Maybe CleanupResults)
mplace mb pill = fmap summarizeClearResults <$> mplaceDetails mb pill

-- | Overwrite the cells under a 'Pill', then repeatedly clear four-in-a-rows
-- and drop unsupported pieces. N.B. nothing will drop if nothing clears, so it
-- is the caller's responsibility to ensure that the pill would be supported
-- where it's being placed.
--
-- Returns 'Nothing' (and does nothing else) if the 'Pill' is out of bounds or
-- over a non-'Empty' cell.
mplaceDetails :: forall m. PrimMonad m => MBoard (PrimState m) -> Pill -> m (Maybe ClearResults)
mplaceDetails mb pill = do
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

-- | Doesn't check that the positions are sensible.
munsafePlace :: PrimMonad m => MBoard (PrimState m) -> Position -> Position -> PillContent -> m ClearResults
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
	unsafeClearAndDrop mb ps

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

-- TODO: doesn't this handle overwriting half of a horizontal pill incorrectly?
-- | Doesn't check that the columns are in-bounds.
munsafeGarbage :: PrimMonad m => MBoard (PrimState m) -> Map Int Color -> m DropResults
munsafeGarbage mb pieces = M.traverseWithKey go pieces >>= unsafeDropAndClear mb
	where
	y = mheight mb - 1
	go x col = p <$ munsafeSet mb p (Occupied col Disconnected) where
		p = Position x y

-- TODO: who uses this? this doesn't disconnect pill halves, which seems weird
-- | Set the given positions to 'Empty', then apply gravity and clear
-- four-in-a-rows.
mclear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m DropResults
mclear mb ps = do
	for_ ps $ \p -> mset mb p Empty
	unsafeDropAndClear mb
		[ p { y = y' }
		| p <- toList ps
		, let y' = y p + 1
		, x p >= 0 && x p < mwidth mb && y p >= 0 && y' < mheight mb
		]

-- | Functions in this module will only produce non-empty 'Map's.
data ClearResults = Clear (Map Position OCell) DropResults | NoClear deriving (Eq, Ord, Read, Show)

-- | Functions in this module will only produce non-empty 'Map's. However, they
-- may produce length-0 drops. In particular, when only half of a pill is
-- involved in a clear, there will be a length-0 drop of a 'Disconnected' cell
-- for the half that didn't clear. (We make a half-hearted attempt to avoid
-- reporting other 0-length drops, but do not guarantee full success.)
data DropResults = Drop (Map Position (Int, OCell)) ClearResults | NoDrop deriving (Eq, Ord, Read, Show)

-- | @unsafeClear board positions@ takes a board and a collection of positions
-- on the board which have recently changed, and modifies the board to take
-- account of four-in-a-rows that involve the given positions. It returns a set
-- of positions which may need gravity applied to them and the contents of any
-- cells that were actually cleared.
--
-- It is the caller's responsibility to ensure that the given positions are in
-- bounds. Under that assumption, the returned positions definitely will be.
unsafeClear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m (Set Position, Map Position OCell)
unsafeClear mb ps = do
	(clears_, disconnects_, drops_) <- unzip3 <$> mapM clearSingleMatch (toList ps)
	let clears = M.unions clears_
	    clearsSet = M.keysSet clears
	    disconnects = S.unions disconnects_ `S.difference` clearsSet
	    drops = (S.unions drops_ `S.difference` clearsSet) `S.union` disconnects
	forM_ clearsSet $ \p -> munsafeSet mb p Empty
	forM_ disconnects $ \p -> munsafeModify mb p disconnect
	return (drops, clears)
	where
	disconnect (Occupied color shape) = Occupied color Disconnected
	-- do we want to check that the shape was not Virus or Disconnected before
	-- and throw an error then, too?
	disconnect Empty = error "The impossible happened: a pill was connected to an empty space."

	-- TODO: coalesce positions that need to be cleared before calling
	-- clearSingleCell on them all, maybe (i.e. just return clears here, and do
	-- an S.fromList in the caller)
	clearSingleMatch p = do
		-- the ~ avoids an annoying MonadFail constraint, and should be safe in
		-- this case
		~[l,r,u,d] <- mapM (mrunLength mb p) [left, right, up, down]
		let clears = [p { x = x p + dx } | l+r+1 >= 4, dx <- [-l .. r]]
		          ++ [p { y = y p + dy } | u+d+1 >= 4, dy <- [-d .. u]]
		(old, disconnects, drops) <- unzip3 <$> mapM clearSingleCell clears
		return (M.unions old, S.unions disconnects, S.unions drops)

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
		return (maybe M.empty (M.singleton p) (toOCell old), disconnects, drops)

-- During unsafeDrop, it may be that we process the same position twice: once
-- as a result of dropping things above something that fell, and once as a
-- result of the caller asking us to. We want to remember the original cell
-- that was dropped, so simply using (<>) i.e. M.union to combine our Maps is
-- fragile (though it probably does work).
--
-- It *should* be the case that on the second and later visits, whatever is at
-- that position doesn't fall. So this type retains whichever cell is
-- associated with the longer fall. If the falls are equal length, that's
-- because they're both 0 -- and so should have the same cell anyway, so it's
-- fine to keep either one in that case.
--
-- The Semigroup instance (ab)uses the fact that the fall distance is the fst
-- part of the map value, so double-check there if you change this newtype
-- definition.
newtype LongestFall = LongestFall (Map Position (Int, OCell)) deriving (Eq, Ord, Read, Show)
instance Semigroup LongestFall where LongestFall m <> LongestFall m' = LongestFall (M.unionWith max m m')
instance Monoid LongestFall where mempty = LongestFall M.empty

singleFall :: Position -> OCell -> Int -> LongestFall
singleFall pos cell fallTime = LongestFall (M.singleton pos (fallTime, cell))

-- | @unsafeDrop board positions@ takes a board and a collection of positions
-- on the board which may have recently become unsupported, and modifies the
-- board to take account of gravity for the given positions. It returns a set
-- of positions which may need to be involved in clears.
--
-- It is the caller's responsibility to ensure that the given positions are in
-- bounds. Under that assumption, the returned positions definitely will be.
unsafeDrop :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> WriterT LongestFall m (Set Position)
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
			Occupied c s -> let ohere = OCell c s in case s of
				Virus -> return S.empty
				East  -> dropDouble p (unsafeMove left  p) here ohere
				West  -> dropDouble p (unsafeMove right p) here ohere
				_ -> do
					dy <- mcolorRunLength mb p down Nothing
					-- Call tell even when dy == 0. This is because we want
					-- some way of reporting which connected cells got
					-- disconnected; we do this by reporting that a
					-- Disconnected cell fell 0 from the position we
					-- disconnected.
					tell (singleFall p ohere dy)
					if dy <= 0 then return S.empty else do
						let p'  = p { y = y p - dy }
						    y'' = y p + 1
						    p'' = p { y = y'' }
						munsafeSet mb p  Empty
						munsafeSet mb p' here
						if y'' >= mheight mb
							then return (S.singleton p')
							else S.insert p' <$> dropSingle p''

	dropDouble p p' here ohere = do
		there <- munsafeGet mb p'
		case there of
			-- should never happen, but...
			Empty -> do
				munsafeSet mb p (Occupied (ocolor ohere) Disconnected)
				dropSingle p
			Occupied c s -> let othere = OCell c s in do
				dy_  <- mcolorRunLength mb p  down Nothing
				dy_' <- mcolorRunLength mb p' down Nothing
				let dy = min dy_ dy_'
				-- Don't need to call tell when dy == 0, because we're mucking about
				-- with two cells that are connected horizontally, hence definitely not
				-- cells that became disconnected.
				if dy <= 0 then return S.empty else do
					let pDown  = p  { y = y p  - dy }
					    pDown' = p' { y = y p' - dy }
					    yUp = y p + 1
					    pUp  = p  { y = yUp }
					    pUp' = p' { y = yUp }
					    drops = S.fromList [pDown, pDown']
					munsafeSet mb p  Empty
					munsafeSet mb p' Empty
					munsafeSet mb pDown  here
					munsafeSet mb pDown' there
					tell (singleFall p  ohere  dy)
					tell (singleFall p' othere dy)
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

-- | A compact summary of what happened during the cleanup phase: how many
-- viruses were cleared, and how many rows of fall time were incurred after
-- each clear. There will always be (exactly) one entry in 'rowsFallen' for
-- each round of clearing there is; in particular this means that its length
-- tells you the number of times a clear animation happened.
data CleanupResults = CleanupResults
	{ clears :: {-# UNPACK #-} !Int
	, rowsFallen :: [Int]
	} deriving (Eq, Ord, Read, Show)

instance Monoid CleanupResults where mempty = CleanupResults 0 []
instance Semigroup CleanupResults where
	CleanupResults c ft <> CleanupResults c' ft' = CleanupResults (c+c') (ft <> ft')

summarizeClearResults :: ClearResults -> CleanupResults
summarizeClearResults = \case
	NoClear -> mempty
	Clear m dr -> CleanupResults
		{ clears = M.size (M.filter ((Virus==) . oshape) m) + clears drSummary
		, rowsFallen = case rowsFallen drSummary of [] -> [0]; other -> other
		} where
		drSummary = summarizeDropResults dr

summarizeDropResults :: DropResults -> CleanupResults
summarizeDropResults = \case
	NoDrop -> mempty
	Drop m cr | rows <= 0 -> mempty
	          | otherwise -> CleanupResults
	          	{ clears = clears crSummary
	          	, rowsFallen = rows : rowsFallen crSummary
	          	} where
		crSummary = summarizeClearResults cr
		Max rows = foldMap' (Max . fst) m

-- | Loop applying gravity and clearing 4-in-a-rows, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeDropAndClear :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m DropResults
unsafeDropAndClear mb ps = do
	(ps', LongestFall falls) <- runWriterT (unsafeDrop mb ps)
	if M.null falls
		then pure NoDrop
		else Drop falls <$> unsafeClearAndDrop mb ps'

-- | Loop clearing 4-in-a-rows and applying gravity, until no changes are made.
--
-- Caller is responsible for ensuring that the positions provided are in
-- bounds.
unsafeClearAndDrop :: (Foldable f, PrimMonad m) => MBoard (PrimState m) -> f Position -> m ClearResults
unsafeClearAndDrop mb ps = do
	(ps', clearMap) <- unsafeClear mb ps
	if M.null clearMap
		then pure NoClear
		else Clear clearMap <$> unsafeDropAndClear mb ps'

-- | An implementation of the random number generator. Given a current RNG
-- state, produces the next one.
advanceRNG :: Word16 -> Word16
advanceRNG seed = seed `shiftR` 1 .|. if testBit seed 1 == testBit seed 9 then 0 else bit 15

-- | Back the RNG state up one step. @advanceRNG . retreatRNG@ and @retreatRNG
-- . advanceRNG@ are almost the identity -- they may toggle the bottom bit.
retreatRNG :: Word16 -> Word16
retreatRNG seed = shiftL (seed `xor` ((seed `xor` shiftR seed 8 `xor` shiftR seed 15) .&. 1)) 1

-- | A little over half of possible 'Word16's cannot be the output of
-- 'advanceRNG'. This function is just like 'retreatRNG', except that it
-- promises to produce an 'advanceRNG' output and is a little bit slower.
retreatRNG' :: Word16 -> Word16
retreatRNG' = advanceRNG . retreatRNG . retreatRNG

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

-- | Initialize a pill's content in the orientation that Dr. Mario launches
-- them in on the NES.
launchContent :: Lookahead -> PillContent
launchContent = pillContentFromLookahead startingOrientation

-- | Initialize a pill in the position and orientation that Dr. Mario launches
-- them in on the NES.
launchPill :: Lookahead -> Pill
launchPill lk = Pill (launchContent lk) startingBottomLeftPosition

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

-- | @randomLevel seed level@ generates lookaheads and a random board in
-- exactly the same way that Dr. Mario would, starting from RNG state given by
-- @seed@. Since lookaheads are generated first, be aware that this will give a
-- different board than @randomBoard seed level@!
randomLevel :: Word16 -> Int -> (V.Vector Lookahead, Board)
randomLevel seed level = runST $ do
	g <- mnewRNG seed
	lks <- mrandomLookaheads g
	mb <- munsafeRandomBoard g level
	b <- mfreeze mb
	pure (lks, b)

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
mrandomBoard seed level = mnewRNG seed >>= flip munsafeRandomBoard level

-- | Given a random number generator (see also 'mnewRNG') and a virus level
-- (usually in the range 0-24), generate a random board of the standard size in
-- the same way Dr. Mario does.
--
-- This is unsafe because on higher levels, it's possible for bad early choices
-- to make it impossible to place the required number of viruses. This function
-- loops forever when that happens.
munsafeRandomBoard :: PrimMonad m => m Word16 -> Int -> m (MBoard (PrimState m))
munsafeRandomBoard mrng level = do
	mb <- memptyBoard 8 16
	munsafeRandomViruses mb virusCount (mrandomPosition height mrng) (mrandomColor mrng)
	return mb
	where
	height     = max 9 . min 12 $ (level+5) `shiftR` 1
	virusCount = max 4 . min 84 $ (level+1) `shiftL` 2

-- | Combines 'mrandomLookaheads' and 'mrandomBoard'.
mrandomLevel :: PrimMonad m => Word16 -> Int -> m (V.Vector Lookahead, MBoard (PrimState m))
mrandomLevel seed level = mnewRNG seed >>= flip munsafeRandomLevel level

-- | Combines 'mrandomLookaheads' and 'munsafeRandomBoard'.
munsafeRandomLevel :: PrimMonad m => m Word16 -> Int -> m (V.Vector Lookahead, MBoard (PrimState m))
munsafeRandomLevel mrng level = liftA2 (,)
	(mrandomLookaheads mrng)
	(munsafeRandomBoard mrng level)

-- | Turn a random number generator action into an action that produces a
-- random 'Lookahead' in the same weird way that Dr. Mario does.
mrandomLookahead :: Monad m => m Word16 -> Word16 -> m (Word16, Lookahead)
mrandomLookahead mrng pc = do
	seed <- mrng
	let pc' = ((shiftR seed 8 .&. 0xf) + pc) `mod` 9
	pure (pc', lookaheadTable V.! fromIntegral pc')

-- | The nine possible (horizontal) pills.
lookaheadTable :: V.Vector Lookahead
lookaheadTable = V.fromListN 9
	[ Lookahead l r
	| l <- [Yellow, Red, Blue]
	, r <- [Yellow, Red, Blue]
	]

-- | Generate 128 pills in exactly the same way Dr. Mario does. See e.g.
-- 'mnewRNG' for the first argument.
mrandomLookaheads :: PrimMonad m => m Word16 -> m (V.Vector Lookahead)
mrandomLookaheads mrng = do
	mpcs <- MV.new 128
	let go m (-1) = mrandomLookahead mrng m >>= MV.unsafeWrite mpcs 127 . snd
	    go m i = do
	    	(m', pc) <- mrandomLookahead mrng m
	    	MV.unsafeWrite mpcs i pc
	    	go m' (i-1)
	go 0 126
	V.unsafeFreeze mpcs

randomLookaheads :: Word16 -> V.Vector Lookahead
randomLookaheads seed = runST (mnewRNG seed >>= mrandomLookaheads)

-- | A monomorphic 'foldMap'. No promises about what order the 'Cell's are
-- visited in.
ofoldMap :: Monoid a => (Cell -> a) -> Board -> a
ofoldMap f = foldMap (U.foldr ((<>) . f) mempty) . cells

-- | Visit all the cells in an unspecified order, yielding their position and
-- value.
ofoldMapWithKey :: Monoid a => (Position -> Cell -> a) -> Board -> a
ofoldMapWithKey f = V.ifoldr (\x -> flip (U.ifoldr (\y -> (<>) . f (Position x y)))) mempty . cells

countVirusesInjection :: Cell -> Sum Int
countVirusesInjection = \case
	Occupied _ Virus -> 1
	_ -> 0

-- | Does what it sounds like it does.
countViruses :: Board -> Int
countViruses = getSum . ofoldMap countVirusesInjection

-- | A monomorphic 'foldMap'-alike. No promises about what order the 'Cell's
-- are visited in.
mofoldMap :: (PrimMonad m, Monoid a) => (Cell -> a) -> MBoard (PrimState m) -> m a
mofoldMap f MBoard { mcells = v } = go (MU.length v - 1) where
	go (-1) = pure mempty
	go i = liftA2 ((<>) . f) (MU.unsafeRead v i) (go (i-1))

mofoldMapWithKey :: (PrimMonad m, Monoid a) => (Position -> Cell -> a) -> MBoard (PrimState m) -> m a
mofoldMapWithKey f b = go (MU.length v - 1) where
	go (-1) = pure mempty
	go i = liftA2 ((<>) . f (indexToPos i)) (MU.unsafeRead v i) (go (i-1))

	h = mheight b
	v = mcells b
	indexToPos i = Position { x = x_, y = y_ } where
		(x_, y_) = i `quotRem` h

mcountViruses :: PrimMonad m => MBoard (PrimState m) -> m Int
mcountViruses = fmap getSum . mofoldMap countVirusesInjection

-- | A monomorphic 'fmap'. It is unsafe because it does not perform clears or
-- gravity after the map.
unsafeMap :: (Cell -> Cell) -> Board -> Board
unsafeMap f b = b { cells = V.map (U.map f) (cells b) }
