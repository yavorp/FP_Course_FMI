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
group [] = []
group (x:xs) = (x : takeWhile (==x) xs) : group (dropWhile (/=x) xs)

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy f x (y:ys) | f x y == LT  = x:(y:ys)
                    | otherwise = y : insertBy f x ys

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy fn (x:xs) = insertBy fn x (sortBy fn xs) 

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy fn (x:xs) = (x : takeWhile (fn x) xs) : groupBy fn (dropWhile (fn x) xs)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on fn f x y = fn (f x) (f y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) fn f x  = (fn x, f x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn fn xs = reverse (map fst sortedArr)
  where
    mapedArr = map (\y -> (y, fn y)) xs
    sortedArr = sortBy (\x y -> snd y `compare` snd x) mapedArr

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn fn xs = map (map fst) groupedArr
    where
      mapedArr = map (\y -> (y, fn y)) xs
      groupedArr = groupBy (\x y -> snd x == snd y) mapedArr

unique' :: Eq a => [a] -> [a]
unique' [] = []
unique' (x:xs) = x : unique' (filter (/= x) xs)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn _ [] = []
classifyOn fn xs = map (\x -> map fst (filter (\y -> snd y == x) mapedArr))  classes
  where classes = unique' (map fn xs)
        mapedArr = map (\x -> (x, fn x)) xs