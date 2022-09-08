module Dr.Mario.Pathfinding (
	BoxMove(..),
	unsafeApproxReachable,
	munsafeApproxReachable,
	) where

import Control.Applicative
import Control.Monad.Loops
import Control.Monad.Primitive
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Dr.Mario.Model
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
			 else if el && not (ed && edl) then horizPillsP (Position xl y) dxl dy else []
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

	PillContent { bottomLeftColor = lc, otherColor = rc } = content p
	horizPillsP = horizPills lc rc
	vertPillsP  = vertPills  lc rc

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
				   else if el && not (ed && edl) then horizPillsP (Position xl y) dxl dy else []
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

	PillContent { bottomLeftColor = lc, otherColor = rc } = content p
	horizPillsP = horizPills lc rc
	vertPillsP  = vertPills  lc rc

	munsafeIsEmpty x y = (Empty==) <$> munsafeGet mb (Position x y)

{-# INLINE horizPills #-}
{-# INLINE  vertPills #-}
horizPills, vertPills :: Color -> Color -> Position -> Int -> Int -> [(Pill, BoxMove)]
horizPills lc rc = \pos dx dy -> tail $ [undefined
	, (Pill pc  pos, BoxMove Clockwise dx dy (Just Counterclockwise))
	, (Pill pc' pos, BoxMove Clockwise dx dy (Just Clockwise))
	] where
	pc  = PillContent Horizontal lc rc
	pc' = PillContent Horizontal rc lc

vertPills lc rc = \pos dx dy -> tail $ [undefined
	, (Pill pc  pos, BoxMove Counterclockwise dx dy Nothing)
	, (Pill pc' pos, BoxMove        Clockwise dx dy Nothing)
	] where
	pc  = PillContent Vertical lc rc
	pc' = PillContent Vertical rc lc

smallerBox :: BoxMove -> BoxMove -> BoxMove
smallerBox m m' = if abs (xDelta m) < abs (xDelta m') then m else m'

-- | Like 'liftA2', but with a 'Control.Monad.join' at the end.
liftJ2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJ2 f ma mb = do
	a <- ma
	b <- mb
	f a b
