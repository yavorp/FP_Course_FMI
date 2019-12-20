module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

group :: Eq a => [a] -> [[a]]
group = undefined

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy f x [] = [x]
insertBy f x (y:ys) | (f x y) == LT  = x:(y:ys)
                    | otherwise = y : (insertBy f x ys)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy fn (x:xs) = insertBy fn x (sortBy fn xs) 

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy fn (x:xs) = (x : (takeWhile (\y -> fn x y) xs)) : (groupBy fn (dropWhile (\y -> fn x y) xs))

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on fn f x y = fn (f x) (f y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) fn f x  = (fn x, f x)

-- sortOn' :: Ord b => [(a, b)] -> [(a,b)]
-- sortOn' :: [] = []
-- sortOn' :: 

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn fn xs = reverse (map (\x -> fst x) sortedArr)
  where
    mapedArr = map (\y -> (y, fn y)) xs
    sortedArr = sortBy (\x y -> snd y `compare` snd x) mapedArr

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn fn xs = map (\x -> (map (\y -> fst y) x)) groupedArr
    where
      mapedArr = map (\y -> (y, fn y)) xs
      groupedArr = groupBy (\x y -> snd x == snd y) mapedArr

-- helper _ [] = []
-- helper fn xs = 

unique' :: Eq a => [a] -> [a]
unique' [] = []
unique' (x:xs) = x : (unique' (filter (\y -> y /= x) xs))

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn _ [] = []
classifyOn fn xs = map (\x -> (map (\z -> fst z) (filter (\y -> snd y == x) mapedArr)))  classes
  where classes = unique' (map (\q -> fn q) xs)
        mapedArr = map (\x -> (x, fn x)) xs