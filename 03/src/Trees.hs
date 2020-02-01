{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Empty Empty = True
  (==) (Node x1 l1 r1) (Node x2 l2 r2) = (x1 == x2) && (==) l1 l2 && (==) r1 r2
  (==) Empty Node{} = False
  (==) Node{} Empty = False

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty = Node x Empty Empty
insertOrdered v (Node root left right) | v > root = Node root left (insertOrdered v right)
                                       | otherwise = Node root (insertOrdered v left) right

listToBST :: Ord a => [a] -> Tree a
listToBST = foldr insertOrdered Empty

isBST :: Ord a => Tree a -> Bool
isBST = between Bot Top

-- idea for implementing isBST - delete if you don't want it
data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)

between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
between _ _ Empty = True
between s b (Node x l r) = (Val x >= s) && (Val x <= b) && between s (Val x) l && between (Val x) b r

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST x (Node v l r) | x == v = True
                       | x > v = findBST x r
                       | otherwise = findBST x l

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node root left right) = Node (f root) (mapTree f left) (mapTree f right)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node root left right) = foldTree left `mappend` root `mappend` foldTree right

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree _ Empty = mempty
foldMapTree fn (Node v l r) = foldMapTree fn l `mappend` fn v `mappend` foldMapTree fn r

sumTree :: Num a => Tree a -> a
sumTree Empty =  0
sumTree tree = Data.Monoid.getSum (foldMapTree Data.Monoid.Sum tree)

allTree :: (a -> Bool) -> Tree a -> Bool
allTree fn tree = Data.Monoid.getAll (foldMapTree (Data.Monoid.All . fn) tree)

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node root left right) = treeToList left ++ root : treeToList right

elemTree :: Eq a => a -> Tree a -> Bool
elemTree a tree = Data.Monoid.getAny (foldMapTree (\x -> Data.Monoid.Any (x == a)) tree)

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a 
findPred p tree = Data.Monoid.getFirst (foldMapTree (Data.Monoid.First . onMaybe p) tree)

findAll :: (a -> Bool) -> Tree a -> [a]
findAll p =  foldMapTree (\x -> [x | p x])

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree = undefined

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch = undefined

paths :: Tree a -> [(a, [Direction])]
paths = undefined
