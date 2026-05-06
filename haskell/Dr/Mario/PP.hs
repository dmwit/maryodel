module Dr.Mario.PP where

import Data.Aeson
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Sequence (Seq)
import Numeric
import System.IO

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map as M
import qualified Data.Text as T

-- | Efficiency? What's that?
--
-- This stuff is primarily used for debugging, where performance doesn't matter
-- much. So let's make the code simple and clear wherever possible.
class PP a where pp :: a -> String
class PP1 f where liftPP1 :: (a -> String) -> f a -> String
class PP2 f where liftPP2 :: (a -> String) -> (b -> String) -> f a b -> String

instance PP Int where pp = show
instance PP Bool where
	pp True = "✓"
	pp False = "✗"

instance PP a => PP [a] where pp = pp1
instance PP a => PP (Seq a) where pp = pp1
instance (PP a, PP b) => PP (a, b) where pp = pp2
instance (PP k, PP v) => PP (Map k v) where pp = pp2

instance PP1 [] where liftPP1 ppElem = ppSequence "[]" . map ppElem
instance PP1 Seq where liftPP1 ppElem = ppSequence "<>" . fmap ppElem
instance PP a => PP1 ((,) a) where liftPP1 = liftPP2 pp
instance PP k => PP1 (Map k) where liftPP1 = liftPP2 pp

instance PP2 (,) where liftPP2 ppA ppB (a, b) = ppSequence "()" [ppA a, ppB b]
instance PP2 Map where liftPP2 ppk ppv = ppSequence "{}" . M.mapWithKey (\k v -> ppk k ++ "↦" ++ ppv v)

ppIO :: PP a => a -> IO ()
ppIO a = putStrLn (pp a) >> hFlush stdout

pp1IO :: (PP1 f, PP a) => f a -> IO ()
pp1IO fa = putStrLn (pp1 fa) >> hFlush stdout

pp2IO :: (PP2 f, PP a, PP b) => f a b -> IO ()
pp2IO fab = putStrLn (pp2 fab) >> hFlush stdout

pp1 :: (PP1 f, PP a) => f a -> String
pp1 = liftPP1 pp

pp2 :: (PP2 f, PP a, PP b) => f a b -> String
pp2 = liftPP2 pp pp

ppAeson :: ToJSON a => a -> String
ppAeson a = case toJSON a of
	String t -> T.unpack t
	other -> LBS8.unpack (encode other)

padr :: Int -> String -> String
padr n s = s ++ replicate (n - length s) ' '

padl :: Int -> String -> String
padl n s = replicate (n - length s) ' ' ++ s

elideTo :: Int -> String -> String
elideTo n s = case drop n s of
	[] -> s
	_ -> take (n-3) s ++ "..."

-- | The first argument is split in half, and the second argument is put
-- inside.
surround :: String -> String -> String
surround brackets s = b ++ s ++ e where (b, e) = splitAt (length brackets `div` 2) brackets

-- | Pass delimiters as the first argument.
ppSequence :: Foldable f => String -> f String -> String
ppSequence brackets = surround brackets . intercalate ", " . toList

ppPrecision :: Int -> Float -> String
ppPrecision p n = if isNaN n then "nan" else showFFloat Nothing (fromInteger (round (pow*n))/pow) ""
	where pow = 10^p

-- | @ppPercent 1 = "100%"@
ppPercent :: Float -> String
ppPercent p = (if isNaN p then "nan" else show (round (100*p))) ++ "%"

ppClockwiseRotations :: Int -> String
ppClockwiseRotations n = case n of
	0 -> "  "
	1 -> " ↻"
	2 -> "↻↻"
	3 -> " ↺"
	_ -> "!!"
