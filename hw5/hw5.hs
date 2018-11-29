import Data.Monoid 
import Data.Char
import Data.List
import Data.Foldable

--1
circShiftL :: Int -> [a] -> [a]
circShiftL n [] = []
circShiftL 0 x = x
circShiftL n xs | n > 0 = circShiftL (n-1) ((tail xs)++[head xs])
		| n < 0 = circShiftL (n+1) ((last xs):(init xs))
----------------------------------------------------------------------
--2.1 По списку возвращает список пар -- (индекс, элемент)
indices :: [a] -> [(Integer, a)]
indices xs = zip  [0..] xs 
--2.2 "Обнуляет" элементы данного списка, неудовлетворяющие заданному условию
zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy xs if_ = map zero xs where
	zero x = if if_ x then x else mempty
--2.3
triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum = zipWith3 (\xs ys zs -> xs + ys + zs)
----------------------------------------------------------------------
--3 
--'\NUL' - is minBound::Char
revRange :: (Char,Char) -> [Char] 
revRange = unfoldr fun 
fun (a, b) | b < a = Nothing
           | minBound == b = Just (b, (succ a, b))
           | otherwise = Just (b, (a, pred b))
----------------------------------------------------------------------
--4
seriesK :: Int -> [Rational]
seriesK k = iterate (/toRational k) 1
----------------------------------------------------------------------
--5 Сделайте сортированный список представителем класса Monoid (операция — слияние списков)
newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

instance Ord a => Semigroup (SortedList a) where
  SortedList xs <> SortedList ys = SortedList $ mergeSortedLists xs ys
instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []

----------------------------------------------------------------------

--слияние отсортированных несписков
mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x:xs) (y:ys) | (x <= y) = x : (mergeSortedLists xs (y:ys))
			       | otherwise = y : (mergeSortedLists (x:xs) ys)

----------------------------------------------------------------------

--6
half_1 :: [a]->[a]
half_1 xs = take (length xs `div` 2) xs

half_2 :: [a]->[a]
half_2 xs = drop (length xs `div` 2) xs

--сортировка слиянием
msort :: Ord a => [a] -> SortedList a
msort [] = mempty
msort [x] = SortedList [x]
msort xs = (msort (half_1 xs)) `mappend` (msort (half_2 xs))
----------------------------------------------------------------------
--7
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)
newtype Preorder a      = PreO (Tree a)     deriving (Eq, Show)
newtype Postorder a     = PostO (Tree a)    deriving (Eq, Show)
newtype Levelorder a    = LevelO (Tree a)   deriving (Eq, Show)

--in-order LNR 
{--
1. Если дерево пустое возвращаем mempty
2. Обходим рекурсивно левое поддерево
3. Показываем поле данных текущего узла
4. Обходим рекурсивно правое поддерево
--}
instance Foldable Tree where
	foldMap f Nil = mempty
	foldMap f (Node left root right) = (foldMap f left) <> f root <> (foldMap f right)

--pre-order (NLR)
{--
1. Если дерево пустое возвращаем mempty
2. Показываем поле данных текущего узла
3. Обходим рекурсивно левое дерево
4. Обходим рекурсивно правое дерево
--}
instance Foldable Preorder where
	foldMap f (PreO Nil) = mempty
	foldMap f (PreO(Node left root right)) = f root <> (foldMap f (PreO left)) <> (foldMap f (PreO right))

--post-order LRN
{--
1. Если дерево пустое возвращаем mempty
2. Обходим рекурсивно левое поддерево
3. Обходим рекурсивно правое поддерево
4. Показываем поле данных текущего узла
--}
instance Foldable Postorder where
	foldMap f (PostO Nil) = mempty
	foldMap f (PostO (Node left root right)) = (foldMap f (PostO left)) <> (foldMap f (PostO right)) <> f root

--level-order (обход в ширину)
instance Foldable Levelorder where
	foldMap f tree = fold_map f (bfs [tree]) where
		fold_map f [] = mempty
		fold_map f (x:xs) = f x <> fold_map f xs

		bfs [] = []
		bfs xs = map idNode xs ++ bfs(concat(map lrn xs)) where
			idNode (LevelO (Node _ a _)) = a
			lrn (LevelO (Node Nil _ Nil)) = []
			lrn (LevelO (Node Nil _ r)) = [LevelO r]  
			lrn (LevelO (Node l _ Nil)) = [LevelO l]
			lrn (LevelO (Node l _ r)) = [LevelO l, LevelO r]

  
--tree = Node(Node(Node Nil [1] Nil) [2] (Node Nil [3] Nil)) [4] (Node(Node Nil [6] Nil) [5] (Node Nil [7] Nil))
{--
LNR -> [1,2,3,4,6,5,7]
NLR -> [4,2,1,3,5,6,7]
LRN -> [1,3,2,6,7,5,4]
LevelO -> [4,2,5,1,3,6,7]
--}
