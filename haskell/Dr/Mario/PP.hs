module Dr.Mario.PP where

import Data.Aeson
import Data.Foldable
import Data.List
import Data.Sequence (Seq)

import qualified Data.ByteString.Lazy.Char8 as LBS8
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

instance PP1 [] where liftPP1 ppElem as = "[" ++ intercalate ", " (map ppElem as) ++ "]"
instance PP1 Seq where liftPP1 ppElem = liftPP1 ppElem . toList
instance (PP a, PP b) => PP (a, b) where pp = pp2
instance PP a => PP1 ((,) a) where liftPP1 = liftPP2 pp
instance PP2 (,) where liftPP2 ppA ppB (a, b) = "(" ++ ppA a ++ ", " ++ ppB b ++ ")"

ppIO :: PP a => a -> IO ()
ppIO = putStrLn . pp

pp1IO :: (PP1 f, PP a) => f a -> IO ()
pp1IO = putStrLn . pp1

pp2IO :: (PP2 f, PP a, PP b) => f a b -> IO ()
pp2IO = putStrLn . pp2

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
