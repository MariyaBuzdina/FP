import Data.Char
import Data.List

--1
cyrcShiftL :: Int -> [a] -> [a]
cyrcShiftL n [] = []
cyrcShiftL 0 x = x
cyrcShiftL n xs | n > 0 = cyrcShiftL (n-1) (tail xs)++[head xs]
		| n < 0 = cyrcShiftL (n+1) ((last xs):(init xs))
----------------------------------------------------------------------
--2.1 По списку возвращает список пар -- (индекс, элемент)
indices :: [a] -> [(Integer, a)]
indices xs = zip  [1..toInteger(length xs)] xs 
--2.2 "Обнуляет" элементы данного списка, неудовлетворяющие заданному условию

--2.3
triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum xs ys zs = zipWith3 (\x y z-> x + y + z) xs ys zs
----------------------------------------------------------------------
--3 
revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b
  where g x | x <  a  = Nothing
            | otherwise = Just (x, pred x)
----------------------------------------------------------------------
--4
seriesK :: Int -> [Rational]
seriesK k = iterate (/toRational k) 1
