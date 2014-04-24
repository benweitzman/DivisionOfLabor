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
    , prettyPrint
    , parallelogram 
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

data HexGrid a = HexGrid (Map HexLocation a) deriving (Eq, Show, Functor, Foldable, Traversable)

enumTuples :: (Enum a, Enum b) => (a, b) -> (a, b) -> [(a, b)]
enumTuples (a, b) (c, d) = [(x, y) | x <- [a..c], y <- [b..d]]

fromList :: [(HexLocation, a)] -> HexGrid a
fromList = HexGrid . M.fromList

parallelogram :: Int -> Int -> (HexLocation -> a) -> HexGrid a
parallelogram width height f = fromList . map (\x -> (x, f x)) $ enumTuples (0,0) (width, height)

adjacentLocations :: HexLocation -> HexGrid a -> [HexLocation]
adjacentLocations (q, r) (HexGrid m) = filter (`M.member` m) 
                                              [(q, r - 1) -- northwest
                                              ,(q + 1, r - 1) -- northeast
                                              ,(q + 1, r) -- east
                                              ,(q, r + 1) -- southeast
                                              ,(q - 1, r + 1) -- southwest
                                              ,(q - 1, r) -- west
                                              ]

maxBy :: (HexLocation -> HexLocation -> Ordering) -> HexGrid a -> HexLocation
maxBy f (HexGrid grid) = maximumBy f $ M.keys grid

minBy :: (HexLocation -> HexLocation -> Ordering) -> HexGrid a -> HexLocation
minBy f (HexGrid grid) = minimumBy f $ M.keys grid

maxQ :: HexGrid a -> HexLocation
maxQ = maxBy (comparing fst)

minQ :: HexGrid a -> HexLocation
minQ = minBy (comparing fst)

maxR :: HexGrid a -> HexLocation
maxR = maxBy (comparing snd)

minR :: HexGrid a -> HexLocation
minR = minBy (comparing snd)

hexWidth :: Int
hexWidth = 9

hexHeight :: Num a => a
hexHeight = 6

toXY :: HexLocation -> (Int, Int)
toXY (q, r) = (8 * q + 4 * r, 4 * r)

type CharGrid = Vector (Vector Char)

update2' :: CharGrid -> ((Int, Int), Char) -> CharGrid
update2' grid ((y, x), c) = grid V.// [(y, grid V.! y V.// [(x, c)])]

update2 :: CharGrid -> [((Int, Int), Char)]  -> CharGrid
update2 = foldl update2'

{-|
  The 'prettyPrint' function will print a hex grid
  using a supplied function to generate the inside of each
  hexagon

  The real estate that the supplied function has to work with
  is as shown:

@

|---9---|  ---
   \/*\\      |
 \/*****\\    |
|*******|   6
|*******|   |
 \\*****\/    |
   \\*\/     _|_
@

  That's

@
  1
  5
  7
  7
  5
  1
@

  characters per row, respectively
-}

makeFill :: (a -> String) -> a -> [((Int, Int), Char)]
makeFill f x = concat updateRows
    where updateRows = map (\((takeN, dropN), rowNum) -> take takeN . drop dropN . zipWith (\x (y, c) -> ((y, x), c)) [0..] . map (\c -> (rowNum, c)) $ rows !! rowNum)
                           (zip rowData [0..])
          rows = lines $ f x
          rowData = [(1, 4), (5, 2), (7, 1), (7, 1), (5, 2), (1, 4)]

prettyPrint :: (a -> String) -> HexGrid a -> String                                              
prettyPrint f g@(HexGrid grid) = toString $ M.foldWithKey printHex blankCharGrid grid
    where printHex :: HexLocation -> a -> CharGrid -> CharGrid
          printHex p _ c = c `update2` ([((hexY,hexX+3), '/')
                                       ,((hexY,hexX+5), '\\')
                                       ,((hexY+1,hexX+1), '/')
                                       ,((hexY+1,hexX+7), '\\')
                                       ,((hexY+2,hexX), '|')
                                       ,((hexY+2,hexX+8), '|')
                                       ,((hexY+3,hexX), '|')
                                       ,((hexY+3,hexX+8), '|')
                                       ,((hexY+4,hexX+1), '\\')
                                       ,((hexY+4,hexX+7), '/')
                                       ,((hexY+5,hexX+3), '\\')
                                       ,((hexY+5,hexX+5), '/')
                                       ] ++ map (\((y, x), c) -> ((y+hexY, x+hexX), c)) (makeFill f (grid ! p)))
            where (hexX, hexY) = toXY p
          blankCharGrid :: CharGrid
          blankCharGrid = V.replicate gridHeight (V.replicate gridWidth ' ')
          gridHeight = maxY - minY + hexHeight
          gridWidth = maxX - minX + hexWidth
          minX = min (fst $ toXY qMin) (fst $ toXY rMin)
          minY = min (snd $ toXY qMin) (snd $ toXY rMin)
          maxX = max (fst $ toXY qMax) (fst $ toXY rMax)
          maxY = max (snd $ toXY qMax) (snd $ toXY rMax)
          qMax = maxQ g
          qMin = minQ g
          rMax = maxR g
          rMin = minR g
          toString :: CharGrid -> String
          toString = concat . V.toList . V.map (\x -> V.toList x ++ "\n")
