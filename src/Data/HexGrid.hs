{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

{-|
Module      : Data.HexGrid
Description : Representation, utilities, and printing for hexagonal grids
Copyright   : (c) Ben Weitzman
License     : MIT
Maintainer  : benweitzman@gmail.com
Stability   : experimental
Portability : non-portable

Data.HexGrid provides the HexGrid type for representing and working with
hexagonal grids, specifically pointy topped hexagons

@
       \/ \\     \/ \\
     \/     \\ \/     \\
    |  0,-1 |  1,-1 |
    |   -R  |       |
   \/ \\     \/ \\     \/ \\
 \/     \\ \/     \\ \/     \\
| -1,0  |  0,0  |  1,0  |
|  -Q   |       |   +Q  |
 \\     \/ \\     \/ \\     \/
   \\ \/     \\ \/     \\ \/
    | -1,1  |  0,1  |
    |       |   +R  |
     \\     \/ \\     \/
       \\ \/     \\ \/
@

-}
module Data.HexGrid
    ( HexGrid
    , HexLocation
    , parallelogram 
    , adjacentLocations
    , ofLocation
    , Direction (..)
    , maxBy
    , minBy
    , minQ
    , maxQ
    , minR
    , maxR
    )
where

import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Traversable as T
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List
import Data.Ord

import Control.Arrow

type HexLocation = (Int, Int)

type HexGrid a = Map HexLocation a

data Direction = NorthEast
               | East
               | SouthEast
               | SouthWest
               | West
               | NorthWest

enumTuples :: (Enum a, Enum b) => (a, b) -> (a, b) -> [(a, b)]
enumTuples (a, b) (c, d) = [(x, y) | x <- [a..c], y <- [b..d]]

fromList :: [(HexLocation, a)] -> HexGrid a
fromList = M.fromList

parallelogram :: Int -> Int -> (HexLocation -> a) -> HexGrid a
parallelogram width height f = fromList . map (\x -> (x, f x)) $ enumTuples (0,0) (width, height)

adjacentLocations :: HexLocation -> HexGrid a -> [HexLocation]
adjacentLocations p m = filter (`M.member` m) . map (`ofLocation` p) $ [NorthEast, NorthWest, East, West, SouthEast, SouthWest]

tileInLocation :: HexLocation -> HexGrid a -> Maybe a
tileInLocation = M.lookup

ofLocation :: Direction -> HexLocation -> HexLocation
NorthEast `ofLocation` (q, r) = (q + 1, r - 1)
East `ofLocation` (q, r) = (q + 1, r)
SouthEast `ofLocation` (q, r) = (q, r + 1)
SouthWest `ofLocation` (q, r) = (q - 1, r + 1)
West `ofLocation` (q, r) = (q - 1, r)
NorthWest `ofLocation` (q, r) = (q , r - 1)

maxBy :: (HexLocation -> HexLocation -> Ordering) -> HexGrid a -> HexLocation
maxBy f grid = maximumBy f $ M.keys grid

minBy :: (HexLocation -> HexLocation -> Ordering) -> HexGrid a -> HexLocation
minBy f grid = minimumBy f $ M.keys grid

maxQ :: HexGrid a -> HexLocation
maxQ = maxBy (comparing fst)

minQ :: HexGrid a -> HexLocation
minQ = minBy (comparing fst)

maxR :: HexGrid a -> HexLocation
maxR = maxBy (comparing snd)

minR :: HexGrid a -> HexLocation
minR = minBy (comparing snd)

