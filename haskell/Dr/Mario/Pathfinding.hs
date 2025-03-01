{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Dr.Mario.Pathfinding (
	BoxMove(..),
	unsafeApproxReachable,
	munsafeApproxReachable,
	smallerBox,
	MidPlacement(..), mpPill,
	MidPath(..),
	MidStep(..),
	HDirection(..),
	mapproxReachable,
	shorterPath,
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Contravariant
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Word
import GHC.Arr
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import qualified Data.HashMap.Strict as HM

-- | A simplified concept of what paths pills can take, named after the shape
-- the pill describes while following it. This path consists of the following
-- moves in sequence:
--
-- 1. rotate
-- 2. move sideways repeatedly
-- 3. drop some number of rows
-- 4. optionally rotate
-- 5. press down to lock in place
--
-- It is assumed that forced drops do not happen during this sequence; in this
-- sense, when the pill speed is very high, it can describe motions that are
-- not actually possible.
--
-- Positive 'xDelta' is to the right; 'yDelta' is never positive and does not include
-- the down-press needed for step 5 above.
data BoxMove = BoxMove
	{ initialRotation :: !Rotation
	, xDelta :: !Int
	, yDelta :: !Int
	, finalRotation :: Maybe Rotation
	} deriving (Eq, Ord, Read, Show)

instance Hashable BoxMove where
	hashWithSalt s BoxMove{ initialRotation = ir, xDelta = dx, yDelta = dy, finalRotation = fr } = s
		`hashWithSalt` ir
		`hashWithSalt` dx
		`hashWithSalt` dy
		`hashWithSalt` fr

boxMoveToTuple :: BoxMove -> (Rotation, Int, Int, Maybe Rotation)
boxMoveToTuple b = (initialRotation b, xDelta b, yDelta b, finalRotation b)

tupleToBoxMove :: (Rotation, Int, Int, Maybe Rotation) -> BoxMove
tupleToBoxMove (irot, dx, dy, frot) = BoxMove irot dx dy frot

instance ToJSON BoxMove where
	toJSON = toJSON . boxMoveToTuple
	toEncoding = toEncoding . boxMoveToTuple

instance FromJSON BoxMove where
	parseJSON v = tupleToBoxMove <$> parseJSON v

instance ToJSONKey BoxMove
instance FromJSONKey BoxMove

-- | Approximate the reachable pill locations. This may return both false
-- positives (see also the commentary on 'BoxMove' about drop speed) and false
-- negatives (it never finds tucks of any kind -- indeed, 'BoxMove' can't even
-- represent tucks).
--
-- It is unsafe because it assumes the bottom left corner of the provided pill
-- is an in-bounds board position, that the board is empty there, and that the
-- pill is horizontal.
--
-- ...but it's much faster than getting the exact right answer.
unsafeApproxReachable :: Board -> Pill -> HashMap Pill BoxMove
unsafeApproxReachable b p = HM.fromListWith smallerBox $ concatMap dropColumn [lox .. hix] where
	Position x0 y0 = bottomLeftPosition p
	search dx x1 = last . takeWhile (\x -> unsafeIsEmpty x y0) $ [x0, x0+dx .. x1]
	lox = search (-1) 0
	hix = search 1 (width b-1)

	dropColumn x = go y0 (emptyL y0) (emptyR y0) where
		go y el er =
			(if er
			 then if       not (ed && edr) then horizPillsP pos dx dy else []
			 else if el && not (ed && edl) then horizPillsP (Position xl y) dx dy else []
			) ++
			(if ed then go y' edl edr else vertPillsP pos dx dy)
			where
			(edl, ed, edr) = if y > 0
				then (emptyL y', unsafeIsEmpty x y', emptyR y')
				else (False, False, False)
			pos = Position x y
			dy = y-y0
			y' = y-1

		emptyL | x <= 0 = const False
		       | otherwise = unsafeIsEmpty xl
		emptyR | xr >= width b = const False
		       | otherwise = unsafeIsEmpty xr

		dx = x-x0
		dxl = dx-1
		xl = x-1
		xr = x+1

	lk = lookaheadFromPill p
	horizPillsP = horizPills lk
	vertPillsP  = vertPills  lk

	unsafeIsEmpty x y = unsafeGet b (Position x y) == Empty

-- | See also 'unsafeApproxReachable'; all the commentary there applies here,
-- too. Does not mutate its argument.
munsafeApproxReachable :: PrimMonad m => MBoard (PrimState m) -> Pill -> m (HashMap Pill BoxMove)
munsafeApproxReachable mb p = do
	lox <- search (-1) 0
	hix <- search 1 (mwidth mb-1)
	HM.fromListWith smallerBox . concat <$> traverse dropColumn [lox .. hix]
	where
	Position x0 y0 = bottomLeftPosition p
	search dx x1 = fmap last . takeWhileM (\x -> munsafeIsEmpty x y0) $ [x0, x0+dx .. x1]

	dropColumn x = liftJ2 (go y0) (memptyL y0) (memptyR y0) where
		go y el er = do
			(edl, ed, edr) <- if y > 0
				then liftA3 (,,) (memptyL yd) (munsafeIsEmpty x yd) (memptyR yd)
				else pure (False, False, False)
			rest <- if ed then go yd edl edr else pure (vertPillsP pos dx dy)
			pure
				$ (if er
				   then if       not (ed && edr) then horizPillsP pos dx dy else []
				   else if el && not (ed && edl) then horizPillsP (Position xl y) dx dy else []
				  )
				++ rest
			where
			pos = Position x y
			dy = y-y0
			yd = y-1

		memptyL | x <= 0 = const (pure False)
		        | otherwise = munsafeIsEmpty xl
		memptyR | xr >= mwidth mb = const (pure False)
		        | otherwise = munsafeIsEmpty xr

		dx = x-x0
		dxl = dx-1
		xl = x-1
		xr = x+1

	lk = lookaheadFromPill p
	horizPillsP = horizPills lk
	vertPillsP  = vertPills  lk

	munsafeIsEmpty x y = (Empty==) <$> munsafeGet mb (Position x y)

{-# INLINE horizPills #-}
{-# INLINE  vertPills #-}
horizPills, vertPills :: Lookahead -> Position -> Int -> Int -> [(Pill, BoxMove)]
horizPills lk = \pos dx dy -> tail $ [undefined
	, (Pill pc  pos, BoxMove Clockwise dx dy (Just Counterclockwise))
	, (Pill pc' pos, BoxMove Clockwise dx dy (Just Clockwise))
	] where
	pc  = pillContentFromLookahead Horizontal lk
	pc' = pillContentFromLookahead Horizontal (mirror lk)

vertPills lk = \pos dx dy -> tail $ [undefined
	, (Pill pc  pos, BoxMove Counterclockwise dx dy Nothing)
	, (Pill pc' pos, BoxMove        Clockwise dx dy Nothing)
	] where
	pc  = pillContentFromLookahead Vertical lk
	pc' = pillContentFromLookahead Vertical (mirror lk)

smallerBox :: BoxMove -> BoxMove -> BoxMove
smallerBox m m' = if abs (xDelta m) < abs (xDelta m') then m else m'

data HDirection = L | R deriving (Bounded, Enum, Eq, Ord, Read, Show)
	deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via SingleCharJSON HDirection

data MidStep
	= Blink
	| Down
	| MidStep (Maybe HDirection) (Maybe Rotation)
	deriving (Eq, Ord, Read, Show)

data MidPath = MidPath
	{ mpSteps :: [MidStep]
	, mpPathLength :: {-# UNPACK #-} !Int
	} deriving (Eq, Ord, Read, Show)

midPath :: [MidStep] -> MidPath
midPath mps = MidPath mps (length mps)

data MidPlacement = MidPlacement
	{ mpBottomLeft :: !Position
	, mpRotations :: !Int -- ^ 0-3, number of clockwise rotations from initial launch
	} deriving (Eq, Ord, Read, Show)

data MidBoardInfo = MidBoardInfo
	{ mbiSensitive :: !Bool -- | Could we press down on the very first frame?
	, mbiGravity :: !Int
	, mbiWidth :: !Int
	} deriving (Eq, Ord, Read, Show)

data MidRowInfo a = MidRowInfo
	{ mriOccupiedHere :: !a
	, mriOccupiedAbove :: !a
	, mriOccupiedBelow :: !a
	, mriY :: {-# UNPACK #-} !Int
	} deriving (Eq, Ord, Read, Show)

data MidLeafInfo a = MidLeafInfo
	{ mliPath :: [MidStep]
	, mliPathLength :: {-# UNPACK #-} !Int
	, mliX :: {-# UNPACK #-} !Int
	, mliExpX :: !a
	, mliOrientation :: !Orientation
	, mliFramesToForcedDrop :: {-# UNPACK #-} !Int
	, mliForbiddenDirection :: Maybe HDirection
	-- Under normal circumstances, we only ever rotate clockwise when vertical
	-- and counterclockwise when horizontal, to avoid problems with pressing
	-- the same rotation button two frames in a row. The exception is blinking.
	-- Since we need to prevent rotating before a blink and blinking before a
	-- rotate, we need to track both kinds of rotation.
	-- TODO: would bit-packing the orientation, forbidden rotations, and
	-- orientability improve the speed?
	, mliForbiddenClockwise :: !Bool
	, mliForbiddenCounterclockwise :: !Bool
	, mliOrientable :: !Bool
	} deriving (Eq, Ord, Read, Show)

-- TODO: So I guess one way to think of the (Int, Orientation) index into
-- mssCache is that the x position and orientation should just be included in
-- pcompare, and this is an optimization that results in shorter
-- [MidLeafInfo]s. But then the obvious question is, are those the right fields
-- to optimize? Perhaps mliOrientable should be in there, too? Or any of the
-- other fields, I guess. So maybe think about a principled way to make that
-- decision.
type MidFrontierCache s a = STArray s (Int, Orientation) [MidLeafInfo a]

data MidSearchState s a = MidSearchState
	{ mssBoardEnv :: MidBoardInfo
	, mssRowEnv :: MidRowInfo a
	, mssCache :: MidFrontierCache s a
	, mssBoard :: MBoard s
	}

instance Hashable HDirection where hashWithSalt = hashUsing fromEnum
instance Hashable MidStep where
	hashWithSalt s = \case
		Blink -> hashWithSalt s LT
		Down -> hashWithSalt s EQ
		MidStep mdir mrot -> s
			`hashWithSalt` GT
			`hashWithSalt` mdir
			`hashWithSalt` mrot

instance Hashable MidPath where
	-- it should be an invariant that if mpSteps a == mpSteps b, then
	-- mpPathLength a == mpPathLength b
	hashWithSalt s = foldl' hashWithSalt s . mpSteps

instance Hashable MidPlacement where
	hashWithSalt s (MidPlacement bl rots) = s
		`hashWithSalt` bl
		`hashWithSalt` rots

instance SingleChar HDirection where
	toChar = \case
		L -> '←'
		R -> '→'
	fromChar = tail [undefined
		, "←<lL" ~> L
		, "→>rR" ~> R
		]

parseMidStep :: String -> Maybe (MidStep, String)
parseMidStep = \case
	'(':'←':'↻':'↺':')':s -> Just (Blink, s)
	'↓':s -> Just (Down, s)
	'-':s -> Just (pass, s)
	'(':dir:rot:')':s -> liftA2 MidStep (tryDir dir) (tryRot rot) <&> \ms -> (ms, s)
	c:s -> (tryDir c <&> \mdir -> (MidStep mdir Nothing, s))
	   <|> (tryRot c <&> \mrot -> (MidStep Nothing mrot, s))
	where
	tryDir = \case
		'←' -> Just (Just L)
		'→' -> Just (Just R)
		_ -> Nothing
	tryRot = \case
		'↻' -> Just (Just Clockwise)
		'↺' -> Just (Just Counterclockwise)
		_ -> Nothing

midStepFromSource :: ParserSource src => src -> Parser MidStep
midStepFromSource src = srcString src >>= \s -> case parseMidStep s of
	Just (ms, "") -> pure ms
	_ -> mismatch "MidStep" src

midStepsFromSource :: ParserSource src => src -> Parser [MidStep]
midStepsFromSource src = srcString src >>= go where
	go [] = pure []
	go s = case parseMidStep s of
		Just (ms, s') -> (ms:) <$> go s'
		Nothing -> mismatch "[MidStep]" src

instance ToJSON MidStep where
	toJSON = toJSON . ppMidStep
	toJSONList = toJSON . concatMap ppMidStep
	toEncoding = toEncoding . ppMidStep
	toEncodingList = toEncoding . concatMap ppMidStep

instance FromJSON MidStep where
	parseJSON = midStepFromSource
	parseJSONList = midStepsFromSource

instance ToJSONKey MidStep where
	toJSONKey = contramap ppMidStep toJSONKey
	toJSONKeyList = contramap (concatMap ppMidStep) toJSONKey

instance FromJSONKey MidStep where
	fromJSONKey = FromJSONKeyTextParser midStepFromSource
	fromJSONKeyList = FromJSONKeyTextParser midStepsFromSource

instance ToJSON MidPath where
	toJSON = toJSON . mpSteps
	toEncoding = toEncoding . mpSteps

instance FromJSON MidPath where
	parseJSON v = midPath <$> parseJSON v

instance ToJSONKey MidPath where
	toJSONKey = contramap mpSteps toJSONKey
	toJSONKeyList = contramap (map mpSteps) toJSONKey

instance FromJSONKey MidPath where
	fromJSONKey = midPath <$> fromJSONKey
	fromJSONKeyList = map midPath <$> fromJSONKey

midPlacementToTuple :: MidPlacement -> (Position, Int)
midPlacementToTuple mp = (mpBottomLeft mp, mpRotations mp)

midPlacementFromTuple :: (Position, Int) -> MidPlacement
midPlacementFromTuple = uncurry MidPlacement

instance ToJSON MidPlacement where
	toJSON = toJSON . midPlacementToTuple
	toEncoding = toEncoding . midPlacementToTuple

instance FromJSON MidPlacement where
	parseJSON v = midPlacementFromTuple <$> parseJSON v

instance ToJSONKey MidPlacement
instance FromJSONKey MidPlacement

-- TODO: memoize blank rows for 8x16 boards and some sensible range of gravities
-- | Find lots of reachable positions. There should be no false positives, i.e.
-- all the paths returned will actually be realizable under the given
-- conditions. However, it may be possible to arrive at some positions a few
-- frames faster with particularly sneaky tricks, and there may be a few
-- reachable placements this does not find.
--
-- Arguments are the board, whether the game is watching for a down press on
-- the first frame of movement, and the current gravity (in frames per row).
mapproxReachable :: PrimMonad m => MBoard (PrimState m) -> Bool -> Int -> m (HashMap MidPlacement MidPath)
mapproxReachable mb sensitive gravity = stToPrim $ HM.fromListWith shorterPath <$>
	if mwidth mb <= 64
	then midInitialize @Word64  mb sensitive gravity >>= midSearch
	else midInitialize @Integer mb sensitive gravity >>= midSearch

midSearch :: (Bits a, Num a) => MidSearchState s a -> ST s [(MidPlacement, MidPath)]
midSearch mss = do
	(mss', mlis) <- mssAdvanceRow mss
	let mri = mssRowEnv mss'
	    (good, bad) = partition (mliUnoccupied mri) mlis
	(finalizePaths (mriY mri+1) bad ++) <$> case good of
		[] -> pure []
		_ -> do
			-- inserting all of them before expanding any promotes
			-- short-circuiting, and can lead to speedups of around 2x
			traverse_ (mfcInsert (mssCache mss')) good
			traverse_ (expand mss') good
			midSearch mss'

midInitialize :: (Bits a, Num a) => MBoard s -> Bool -> Int -> ST s (MidSearchState s a)
midInitialize mb sensitive gravity = do
	occupiedHere <- getOccupation mb y
	occupiedBelow <- getOccupation mb (y-1)
	cache <- newSTArray ((0, minBound), (xMax, maxBound)) []
	let mss = MidSearchState
	    	{ mssBoardEnv = MidBoardInfo
	    		{ mbiSensitive = sensitive
	    		, mbiGravity = gravity
	    		, mbiWidth = w
	    		}
	    	, mssRowEnv = MidRowInfo
	    		{ mriOccupiedHere = occupiedHere
	    		, mriOccupiedAbove = 0
	    		, mriOccupiedBelow = occupiedBelow
	    		, mriY = y
	    		}
	    	, mssCache = cache
	    	, mssBoard = mb
	    	}
	    mli = MidLeafInfo
	    	{ mliPath = []
	    	, mliPathLength = 0
	    	, mliX = x
	    	, mliExpX = bit x
	    	, mliOrientation = startingOrientation
	    	, mliFramesToForcedDrop = gravity
	    	, mliForbiddenDirection = Nothing
	    	, mliForbiddenClockwise = False
	    	, mliForbiddenCounterclockwise = False
	    	, mliOrientable = False
	    	}
	when ((occupiedHere `shiftR` x) .&. 3 == 0) $ do
		mfcInsert cache mli
		expand mss mli
	pure mss
	where
	w = mwidth mb
	xMax = w-1
	x = xMax`quot`2
	y = mheight mb-1

shorterPath :: MidPath -> MidPath -> MidPath
shorterPath mp mp' = if mpPathLength mp < mpPathLength mp' then mp else mp'

mpPill :: MidPlacement -> Lookahead -> Pill
mpPill mp lk = Pill
	{ bottomLeftPosition = mpBottomLeft mp
	, content = pillContentFromLookahead
		(toEnum (rots .&. 1))
		(if rots == 1 || rots == 2 then mirror lk else lk)
	} where
	rots = mpRotations mp .&. 3

pcompare :: MidLeafInfo a -> MidLeafInfo a -> POrdering
pcompare mli mli' = mempty
	<> p (on compare mliPathLength mli mli')
	<> on go mliForbiddenDirection mli mli'
	<> p (on compare mliForbiddenClockwise mli mli')
	<> p (on compare mliForbiddenCounterclockwise mli mli')
	<> p (on compare mliOrientable mli' mli) -- order flipped!
	-- TODO: 0 or 1 frames to forced drop could occasionally be better than
	-- more frames, because we'd get an extra frame for horizontal movement
	-- next row (but this seems like it will matter rarely enough to fudge for
	-- now); when fixing, remember that a truly correct fix means you should
	-- (almost?) always consider waiting for a forced drop rather than dropping
	-- now, even if there's many frames left to a forced drop, to get that
	-- extra horizontal movement time
	<> p (on compare mliFramesToForcedDrop mli' mli) -- order flipped!
	where
	go (Just dir) (Just dir') = if dir == dir' then PEQ else PIN
	go mdir mdir' = p (compare mdir mdir')

mliTryInsert :: MidLeafInfo a -> [MidLeafInfo a] -> Maybe [MidLeafInfo a]
mliTryInsert mli = go where
	go = \case
		[] -> Just [mli]
		mli':mlis -> case pcompare mli mli' of
			PLT -> Just (mli:filter (pgt . pcompare mli) mlis)
			PEQ -> Nothing
			PGT -> Nothing
			PIN -> (mli':) <$> go mlis

-- mliInsert could be implemented as a straightforward recursion rather than
-- dispatching to the convoluted recursion done in mliTryInsert. That would
-- likely be about as fast, because it would avoid a lot of Maybe allocations
-- but at the cost of an equal number of new cons cell allocations, while being
-- more readable. But in other parts of the code we need mliTryInsert to report
-- whether anything changed, so in the interest of DRY, we just reuse it here.
mliInsert :: MidLeafInfo a -> [MidLeafInfo a] -> [MidLeafInfo a]
mliInsert mli mlis = fromMaybe mlis $ mliTryInsert mli mlis

mliAdvanceRow :: MidBoardInfo -> MidLeafInfo a -> MidLeafInfo a
mliAdvanceRow mbi mli
	| mliFramesToForcedDrop mli == 0 = mli { mliFramesToForcedDrop = mbiGravity mbi }
	| mliFramesToForcedDrop mli == 1 = mli
		{ mliPath = MidStep Nothing Nothing : mliPath mli
		, mliPathLength = mliPathLength mli + 1
		, mliFramesToForcedDrop = mbiGravity mbi
		, mliForbiddenDirection = Nothing
		, mliForbiddenClockwise = False
		, mliForbiddenCounterclockwise = False
		, mliOrientable = mliOrientable mli || mliOrientation mli == Vertical
		}
	| otherwise = mli
		{ mliPath = Down : if sensitiveNow
			then mliPath mli
			else MidStep Nothing Nothing : mliPath mli
		, mliPathLength = mliPathLength mli + if sensitiveNow then 1 else 2
		, mliFramesToForcedDrop = mbiGravity mbi
		, mliForbiddenDirection = Nothing
		, mliForbiddenClockwise = False
		, mliForbiddenCounterclockwise = False
		, mliOrientable = mliOrientable mli || mliOrientation mli == Vertical
		} where sensitiveNow = mliPathLength mli .&. 1 /= fromEnum (mbiSensitive mbi)

-- f: frontier
fAdvanceRow :: MidBoardInfo -> [MidLeafInfo a] -> [MidLeafInfo a]
fAdvanceRow mbi = foldr (mliInsert . mliAdvanceRow mbi) []

fcAdvanceRow :: MidBoardInfo -> MidFrontierCache s a -> ST s [MidLeafInfo a]
fcAdvanceRow mbi = id
	. fmap (sortBy (compare `on` mliPathLength))
	. foldMapSTArray (fAdvanceRow mbi)

mssAdvanceRow :: (Bits a, Num a) => MidSearchState s a -> ST s (MidSearchState s a, [MidLeafInfo a])
mssAdvanceRow mss = do
	cache <- newSTArray ((0, minBound), (xMax, maxBound)) []
	frontier <- fcAdvanceRow (mssBoardEnv mss) (mssCache mss)
	occupiedBelow <- getOccupation (mssBoard mss) (y-1)
	let mss' = mss
	    	{ mssRowEnv = MidRowInfo
	    		{ mriOccupiedHere = mriOccupiedBelow (mssRowEnv mss)
	    		, mriOccupiedAbove = mriOccupiedHere (mssRowEnv mss)
	    		, mriOccupiedBelow = occupiedBelow
	    		, mriY = y
	    		}
	    	, mssCache = cache
	    	}
	pure (mss', frontier)
	where
	xMax = mbiWidth (mssBoardEnv mss) - 1
	y = mriY (mssRowEnv mss) - 1

finalizePaths :: Int -> [MidLeafInfo a] -> [(MidPlacement, MidPath)]
finalizePaths y = concatMap go where
	go mli = (MidPlacement pos rots, MidPath path len)
		: [(MidPlacement pos (rots+2), MidPath (reorient Horizontal path) len) | mliOrientable mli]
		where
		pos = Position (mliX mli) y
		rots = case mliOrientation mli of Horizontal -> 0; Vertical -> 1
		path = reverse (mliPath mli)
		len = mliPathLength mli

	reorient _ [] = []
	reorient ation ms@(mh:mt) = case (ation, mh) of
		(Vertical, Down) -> ms
		(Vertical, MidStep _ Nothing) -> ms
		(_, MidStep mdir (Just rot)) -> MidStep mdir (Just (chiral rot)):reorient (perpendicular ation) mt
		(_, _) -> mh:reorient ation mt

mliUnoccupied :: (Bits a, Num a) => MidRowInfo a -> MidLeafInfo a -> Bool
mliUnoccupied mri mli = mliExpX mli .&. occupiedMask == 0 where
	occupiedMask = mriOccupiedHere mri .|. case mliOrientation mli of
		Vertical -> mriOccupiedAbove mri
		Horizontal -> shiftR (mriOccupiedHere mri) 1

getOccupation :: (Bits a, Num a) => MBoard s -> Int -> ST s a
getOccupation mb (-1) = pure (complement 0)
getOccupation mb y = go (mwidth mb-1) 0 where
	go (-1) occ = pure occ
	go x occ = do
		cell <- munsafeGet mb (Position x y)
		go (x-1) $ shiftL occ 1 .|. case cell of
			Empty -> 0
			_ -> 1

expand :: (Bits a, Num a) => MidSearchState s a -> MidLeafInfo a -> ST s ()
expand mss mli = expandLeft mss mli >> expandRight0 mss mli

-- TODO: Is it possible that, while expanding left, we fail to insert because
-- there's a better solution coming from a previous right expansion, but that
-- if we were to keep going left we might succeed in inserting? For example,
-- perhaps we are orientable, and the thing coming from a previous expansion is
-- orientable now but earlier in the expansion it wasn't. If it's possible,
-- then what is the *right* stopping condition?
--
-- (A similar question applies to expandRight, obviously.)
expandLeft :: (Bits a, Num a) => MidSearchState s a -> MidLeafInfo a -> ST s ()
expandLeft mss mli = when (mliFramesToForcedDrop mli > 0) $
	for_ (leftMotions mli) $ \step ->
		for_ (tryStep (mssBoardEnv mss) (mssRowEnv mss) mli step) $ \mli' ->
			mfcInsertThen mli' mfc (expandLeft mss mli')
	where mfc = mssCache mss

-- We expand left and then expand right. Consequently, moves that are valid
-- prefixes of left or right moves, but which themselves do not contain a left
-- or right move, will be attempted by both expansions. In the expansion to the
-- right, this seems like a duplication of a previously visited state, but
-- isn't, because we plan to follow it up with a right move rather than a left
-- move.
--
-- To combat this false positive in search pruning, we let the first step of
-- expansion to the right skip the duplication check. Note that the "recursive"
-- call is to expandRight, not expandRight0.
--
-- TODO: can we keep the duplication check under more conditions than we do here?
expandRight0 :: (Bits a, Num a) => MidSearchState s a -> MidLeafInfo a -> ST s ()
expandRight0 mss mli = when (mliFramesToForcedDrop mli > 0) $
	for_ (rightMotions mli) $ \step ->
		for_ (tryStep (mssBoardEnv mss) (mssRowEnv mss) mli step) $ \mli' -> do
			mfcInsertThen mli' mfc (pure ())
			expandRight mss mli'
	where mfc = mssCache mss

expandRight :: (Bits a, Num a) => MidSearchState s a -> MidLeafInfo a -> ST s ()
expandRight mss mli = when (mliFramesToForcedDrop mli > 0) $
	for_ (rightMotions mli) $ \step ->
		for_ (tryStep (mssBoardEnv mss) (mssRowEnv mss) mli step) $ \mli' ->
			mfcInsertThen mli' mfc (expandRight mss mli')
	where mfc = mssCache mss

mfcInsert :: MidFrontierCache s a -> MidLeafInfo a -> ST s ()
mfcInsert mfc mli = modifySTArray mfc (mliX mli, mliOrientation mli) (mliInsert mli)

mfcInsertThen :: MidLeafInfo a -> MidFrontierCache s a -> ST s () -> ST s ()
mfcInsertThen mli mfc k = do
	mlis <- readSTArray mfc placement
	for_ (mliTryInsert mli mlis) $ \mlis' -> do
		writeSTArray mfc placement mlis'
		k
	where placement = (mliX mli, mliOrientation mli)

tryStep :: (Bits a, Num a) => MidBoardInfo -> MidRowInfo a -> MidLeafInfo a -> MidStep -> Maybe (MidLeafInfo a)
tryStep mbi mri mli0 step = extendPath <$> do
	let mustDrop = mliFramesToForcedDrop mli0 == 1
	    here = (if mustDrop then mriOccupiedBelow else mriOccupiedHere) mri
	    above = (if mustDrop then mriOccupiedHere else mriOccupiedAbove) mri
	    tryDir = tryDir_ here above
	    tryRot = tryRot_ here above
	when mustDrop (tryDown here above mli0)
	case step of
		Blink -> do
			mli' <- (tryDir L >=> tryRot Clockwise >=> tryRot Counterclockwise >=> tryDir L) mli0
			pure mli'
				{ mliForbiddenDirection = Just L
				, mliForbiddenClockwise = True
				, mliForbiddenCounterclockwise = True -- never read
				}
		MidStep (Just L) (Just rot) | mliOrientation mli0 == Vertical -> do
			mli' <- (tryDir L >=> tryRot rot >=> tryDir L) mli0
			pure mli'
				{ mliForbiddenDirection = Just L
				, mliForbiddenClockwise = rot == Clockwise -- always true
				, mliForbiddenCounterclockwise = rot == Counterclockwise -- always false
				}
		MidStep mdir mrot -> do
			mli' <- (maybe pure tryDir mdir >=> maybe pure tryRot mrot) mli0
			pure mli'
				{ mliForbiddenDirection = mdir
				, mliForbiddenClockwise = mrot == Just Clockwise -- never read
				, mliForbiddenCounterclockwise = mrot == Just Counterclockwise
				-- TODO: this assumes the original pill that we started the
				-- pathfinding from was horizontal
				, mliOrientable = mliOrientable mli0 || (mliOrientation mli0 == Vertical && mrot == Nothing)
				}
		-- TODO: currently we never pass Down, but we should probably support it in
		-- case that changes
		Down -> Nothing
	where
	tryDir_ here above dir mli = do
		guard (mliForbiddenDirection mli /= Just dir)
		guard $ case (mliOrientation mli, dir) of
			(Horizontal, L) -> here .&. shiftR (mliExpX mli) 1 == 0 && mliX mli-1 >= 0
			(Horizontal, R) -> here .&. shiftL (mliExpX mli) 2 == 0 && mliX mli+2 < mbiWidth mbi
			(Vertical  , L) -> (here .|. above) .&. shiftR (mliExpX mli) 1 == 0 && mliX mli-1 >= 0
			(Vertical  , R) -> (here .|. above) .&. shiftL (mliExpX mli) 1 == 0 && mliX mli+1 < mbiWidth mbi
		pure mli
			{ mliExpX = (case dir of L -> shiftR; R -> shiftL) (mliExpX mli) 1
			, mliX = (case dir of L -> pred; R -> succ) (mliX mli)
			}

	tryRot_ here above rot mli = do
		guard . not $ case rot of
			Clockwise -> mliForbiddenClockwise mli
			Counterclockwise -> mliForbiddenCounterclockwise mli
		case mliOrientation mli of
			Horizontal -> do
				guard (above .&. mliExpX mli == 0)
				pure mli { mliOrientation = Vertical }
			Vertical -> case (here .&. shiftR (mliExpX mli) 1, here .&. shiftL (mliExpX mli) 1) of
				(_, 0) | mliX mli+1 < mbiWidth mbi -> pure mli { mliOrientation = Horizontal }
				(0, _) | mliX mli-1 >= 0 -> pure mli
					{ mliExpX = shiftR (mliExpX mli) 1
					, mliX = mliX mli - 1
					, mliOrientation = Horizontal
					}
				_ -> Nothing

	tryDown here above mli = guard $ here .&. (mliExpX mli .|. maybeRight) == 0 where
		maybeRight = case mliOrientation mli of
			Horizontal -> shiftL (mliExpX mli) 1
			Vertical -> 0

	extendPath mli = mli
		{ mliPath = step : mliPath mli
		, mliPathLength = mliPathLength mli + 1
		, mliFramesToForcedDrop = mliFramesToForcedDrop mli - 1
		}

-- It seems like there ought to be a pattern we could abuse here to not have to
-- list these explicitly, but I can't see it. Some principles:
-- * When vertical, only rotate counterclockwise. When horizontal, only rotate
--   clockwise. This reduces the number of forbidden rotation situations, and
--   we'll fix up vertical and orientable placements later to swap rotations.
-- * When not orientable, favor becoming orientable (by rotating when
--   horizontal and not rotating when vertical).
-- * When orientable, favor not rotating (just for aesthetics -- it makes the
--   paths have fewer frenetic rotations).
-- * Favor moving fast to moving, and favor moving to staying still.
leftMotions :: MidLeafInfo a -> [MidStep]
leftMotions mli = case (mliOrientable mli, mliOrientation mli) of
	(False, Horizontal) -> [Blink, leftClock, leftOnly, clock, pass]
	(False, Vertical  ) -> [leftOnly, leftCounter, pass, counter]
	(True , Horizontal) -> [Blink, leftOnly, leftClock, pass, clock]
	(True , Vertical  ) -> [leftCounter, leftOnly, pass, counter]

rightMotions :: MidLeafInfo a -> [MidStep]
rightMotions mli = case (mliOrientable mli, mliOrientation mli) of
	(False, Horizontal) -> [rightClock, rightOnly, clock, pass]
	(_, ation) -> [rightOnly, MidStep (Just R) rot, pass, MidStep Nothing rot]
		where rot = Just $ case ation of Horizontal -> Clockwise; _ -> Counterclockwise

[pass, clock, counter, leftOnly, leftClock, leftCounter, rightOnly, rightClock, rightCounter] = liftA2 MidStep
	[Nothing, Just L, Just R]
	[Nothing, Just Clockwise, Just Counterclockwise]

data POrdering = POrdering
	{ plt :: Bool
	, pgt :: Bool
	} deriving (Eq, Ord, Read, Show)

{-# COMPLETE PLT, PEQ, PGT, PIN #-}
pattern PLT = POrdering True False
pattern PEQ = POrdering False False
pattern PGT = POrdering False True
pattern PIN = POrdering True True

instance Semigroup POrdering where
	po <> po' = POrdering
		{ plt = plt po || plt po'
		, pgt = pgt po || pgt po'
		}

instance Monoid POrdering where
	mempty = POrdering False False

p :: Ordering -> POrdering
p o = POrdering
	{ plt = o == LT
	, pgt = o == GT
	}

foldMapSTArray :: Monoid m => (a -> m) -> STArray s i a -> ST s m
foldMapSTArray f arr = mconcat <$> traverse (fmap f . unsafeReadSTArray arr) [0..numElementsSTArray arr-1]

ifoldMapSTArray :: (Ix i, Monoid m) => (i -> a -> m) -> STArray s i a -> ST s m
ifoldMapSTArray f arr = mconcat <$> zipWithM f' [0..] (range (boundsSTArray arr)) where
	f' ix i = f i <$> unsafeReadSTArray arr ix

modifySTArray :: Ix i => STArray s i a -> i -> (a -> a) -> ST s ()
modifySTArray arr i f = readSTArray arr i >>= writeSTArray arr i . f

-- | Like 'liftA2', but with a 'Control.Monad.join' at the end.
liftJ2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJ2 f ma mb = do
	a <- ma
	b <- mb
	f a b

-- debugging only
ppSingleChar :: SingleChar a => a -> String
ppSingleChar = pure . toChar

ppBool :: Bool -> String
ppBool = \case True -> "✓"; False -> "✗"

ppMidStep :: MidStep -> String
ppMidStep = \case
	Blink -> "(←↻↺)"
	Down -> "↓"
	MidStep dir rot -> case foldMap ppSingleChar dir <> foldMap ppSingleChar rot of
		[] -> "-"
		[c] -> [c]
		ans -> "(" ++ ans ++ ")"

ppBackwardsMidSteps :: [MidStep] -> String
ppBackwardsMidSteps = ppMidSteps . reverse

ppMidSteps :: [MidStep] -> String
ppMidSteps mss = ['ε' | null mss] ++ foldMap ppMidStep mss

ppPOrdering :: POrdering -> String
ppPOrdering = \case
	PLT -> "<"
	PEQ -> "="
	PGT -> ">"
	PIN -> "∥"

ppList :: (a -> String) -> [a] -> String
ppList ppElem as = "[" ++ intercalate "," (map ppElem as) ++ "]"

ppBackwardsBits :: (Bits a, Num a, Show a) => Int -> a -> String
ppBackwardsBits 0 _ = ""
ppBackwardsBits w a = show (a .&. 1) ++ ppBackwardsBits (w-1) (shiftR a 1)

ppBits :: (Bits a, Num a, Show a) => Int -> a -> String
ppBits w = reverse . ppBackwardsBits w

ppBriefMidLeafInfo :: MidLeafInfo a -> String
ppBriefMidLeafInfo mli = ""
	++ ppBackwardsMidSteps (mliPath mli) ++ " "
	++ show (mliFramesToForcedDrop mli) ++ "↓ "
	++ [eraseIf (dir `elem` mliForbiddenDirection mli) (toChar dir) | dir <- [L, R]]
	++ [eraseIf (f mli) (toChar rot) | (rot, f) <- rotationChecks]
	++ ppBool (mliOrientable mli)
	where
	eraseIf b c = if b then ' ' else c

ppMidLeafInfo :: (Bits a, Num a, Show a) => Int -> MidLeafInfo a -> String
ppMidLeafInfo w mli = ""
	++ [toChar (mliOrientation mli), '@']
	++ show (mliX mli) ++ "/" ++ ppBackwardsBits w (mliExpX mli) ++ ": "
	++ ppBackwardsMidSteps (mliPath mli) ++ " ("
		++ show (mliFramesToForcedDrop mli) ++ "↓, "
		++ [toChar dir | dir <- [L, R] \\ toList (mliForbiddenDirection mli)]
		++ [toChar rot | (rot, f) <- rotationChecks, not (f mli)] ++ ", "
		++ ppBool (mliOrientable mli)
	++ ")"

rotationChecks :: [(Rotation, MidLeafInfo a -> Bool)]
rotationChecks = [(Clockwise, mliForbiddenClockwise), (Counterclockwise, mliForbiddenCounterclockwise)]

ppMidRowInfo :: (Bits a, Num a, Show a) => Int -> MidRowInfo a -> String
ppMidRowInfo w mri = ""
	++ show (mriY mri + 1) ++ ": " ++ ppBackwardsBits w (mriOccupiedAbove mri) ++ "; "
	++ show (mriY mri    ) ++ ": " ++ ppBackwardsBits w (mriOccupiedHere  mri) ++ "; "
	++ show (mriY mri - 1) ++ ": " ++ ppBackwardsBits w (mriOccupiedBelow mri)

ppMidBoardInfo :: MidBoardInfo -> String
ppMidBoardInfo mbi = ""
	++ "←" ++ show (mbiWidth mbi) ++ "→ "
	++ "↓₀" ++ ppBool (mbiSensitive mbi) ++ " "
	++ pad 2 (show (mbiGravity mbi)) ++ "/↓"

pad :: Int -> String -> String
pad n s = replicate (n-length s) ' ' ++ s

ppMidSearchState :: (Bits a, Num a, Show a) => MidSearchState s a -> String
ppMidSearchState mss = ""
	++ ppMidBoardInfo (mssBoardEnv mss) ++ "\n"
	++ ppMidRowInfo (mbiWidth (mssBoardEnv mss)) (mssRowEnv mss) ++ "\n"

ppMidSearchStateST :: (Bits a, Num a, Show a) => MidSearchState s a -> ST s String
ppMidSearchStateST mss = do
	b <- mfreeze (mssBoard mss)
	fcLines <- ifoldMapSTArray (\(x, o) mlis -> [[toChar o, '@'] ++ show x ++ ": " ++ ppList ppBriefMidLeafInfo mlis]) (mssCache mss)
	pure $ ""
		++ pp b
		++ ppMidSearchState mss
		++ unlines fcLines

ppMidPlacement :: MidPlacement -> String
ppMidPlacement mp = "(" ++ show (x (mpBottomLeft mp)) ++ ", " ++ pad 2 (show (y (mpBottomLeft mp))) ++ ") " ++ replicate (mpRotations mp) '↻' ++ replicate (3-mpRotations mp) ' '

ppMidPath :: MidPath -> String
ppMidPath = ppMidSteps . mpSteps

ppMidResult :: (MidPlacement, MidPath) -> String
ppMidResult (placement, path) = ppMidPlacement placement ++ ": " ++ ppMidPath path

ppMidResults :: HashMap MidPlacement MidPath -> String
ppMidResults = ppList ppMidResult . sort . HM.toList

ppMidResultsLn :: HashMap MidPlacement MidPath -> String
ppMidResultsLn = unlines . map ppMidResult . sort . HM.toList
