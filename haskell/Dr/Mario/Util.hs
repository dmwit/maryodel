module Dr.Mario.Util where

infixr 3 `andM`
andM :: Monad m => m Bool -> m Bool -> m Bool
andM l r = do
	v <- l
	if v then r else return False

infixr 2 `orM`
orM :: Monad m => m Bool -> m Bool -> m Bool
orM l r = do
	v <- l
	if v then return True else r
