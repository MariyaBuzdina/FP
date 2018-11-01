import Data.List
import Data.Char
------------------------------------------------------------------------------
--FP1
--1)
id_ :: a -> a
id_ x = x
--2)
eval :: (a -> b, a) -> b
eval (g, x) = g x
--3)
exchange :: (a, b) -> (b, a)
exchange (a, b) = (b, a)
--4)
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g h = f(g h)
--5)
curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ f a b = f (a, b)
--6)
associate :: (a, (b, c)) -> ((a, b), c)
associate (a, (b, c)) = ((a, b), c)
------------------------------------------------------------------------------
--FP2
max2 :: (Ord a) => a->a->a
max2 a b | a >= b = a
	 | a < b = b

min2 :: (Ord a) => a->a->a
min2 a b | a >= b = b
	 | a < b = a

min_max_hof :: (Ord a) => (a->a->a)->[a] -> a
min_max_hof f [x] = x
min_max_hof f (x:xs) = f x ( min_max_hof f xs)

minMax :: Ord a => [a] -> Maybe(a, a)
minMax [] = Nothing
minMax (x:xs) = Just(min_max_hof min2 (x:xs), min_max_hof max2 (x:xs))
------------------------------------------------------------------------------
--FP3
sumANDcount :: Integer -> (Integer, Integer)
sumANDcount num =  helper 0 0 (abs num) 
	where 
		helper 0 0 0 = (0, 1)
		helper sum count 0 = (sum, count)
		helper sum count num = helper (sum + mod num 10) (count + 1) $ div num 10
------------------------------------------------------------------------------
--FP4
majority :: Eq a => [a] -> Maybe a
majority xs = result xs >>= majorElem xs

--Ищем одного кандидата
result :: Eq a => [a] -> Maybe a
result []     = Nothing
result (x:xs) = Just (findCandidate x xs)
{--если текущее значение переменной-счётчика равно нулю, то данный элемент 
последовательности записывается в первую переменную, а счётчик становится равен 1.
Если значение счётчика отлично от нуля, то текущий элемент последотельности сравнивается 
со значением, записанным в первую переменную. Если они совпадают, то счётчик увеличивается
на 1, иначе — уменьшается на 1. После прохода функция возвращает элемент, который может быть преобладающим--}
findCandidate:: Eq a => a -> [a] -> a
findCandidate candidate = fst . foldr step (candidate, 1 )
  where
    step x (can, cnt)
      | can == x = (can, succ cnt)
      | cnt == 0 = (x  , 1)
      | otherwise = (can, pred cnt)

-- проверяем, является ли кандидат преобладающим элементом
majorElem :: Eq a => [a] -> a -> Maybe a
majorElem xs can | grHalf can xs = Just can
              	 | otherwise      = Nothing
{--если кандидат совпадает с элементом списка то cnt увеличивается
если нет - уменьшается. Если в итоге cnt > 0, то элемент преобладающий--}
grHalf :: Eq a => a -> [a] -> Bool
grHalf can  = (> 0) . foldr counter (0)
  where
    counter x cnt
      | can == x = succ cnt
      | otherwise = pred cnt
------------------------------------------------------------------------------
--FP5
f :: (a -> a) -> Int -> (a -> a)
f g n
    | n < 0     = error "n must be non-negative number"
    | n == 0    = id_
    | otherwise = (.) g (f g (n - 1))

------------------------------------------------------------------------------
--FP6
lastnumeral :: Integer->Integer
lastnumeral n = mod (fibonacci n) 10

fibonacci :: Integer -> Integer
fibonacci n = helper (0, 1) n where
    helper (a, b) n | n == 0  = a
                    | n == 1 = b
                    | n > 1 = helper(b, a+b) $ n-1
                    | n < 0 = helper(b-a, a) $ n+1
					
------------------------------------------------------------------------------					
--FP7
isPalindrom :: String->Bool
isPalindrom [] = True
isPalindrome [x] = True
isPalindrome (x:xs) | toLower x == toLower (last xs) = isPalindrome (init xs)
		    | otherwise = False
