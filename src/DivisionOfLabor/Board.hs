{-# LANGUAGE FlexibleContexts #-}

{-
 _   _   _   _   _
/0\_/2\_/4\_/ \_/ \
\0/1\0/3\0/x\_/ \_/
/0\0/2\0/4\y/ \_/ \
\1/1\1/3\1/ \_/ \_/
/0\1/2\1/ \_/ \_/ \
\2/ \2/ \_/ \_/ \_/

        
(1,0)(2,0)(3,0)
    \  |  /
     (2,1)
    /  |  \
(1,1)(2,2)(3,1)

(x-1,y-1)(x,y-1)(x+1,y-1)
        \   |   /
          (x,y)
        /   |   \
(x-1,y)  (x,y+1)(x+1,y)

-}

module DivisionOfLabor.Board where

import Data.Map ((!))
import qualified Data.Map as M
import Data.Map (Map)    
import qualified Data.Traversable as T

import Control.Monad.Random
import Control.Monad.State
import Control.Arrow

type BoardLocation = (Int, Int) 

type GameMap = Map BoardLocation TerrainGroup

type GameBoard = Map BoardLocation BoardSpace

data BoardSpace = BoardSpace
    { terrain :: Terrain
    , discovered :: Bool
    , terrainGroup :: TerrainGroup
    } deriving (Eq, Show)

data TerrainGroup = A
                  | B
                  | C
                  | D 
                  deriving (Eq, Ord, Show)

data Terrain = Plains
             | Hills
             | Mountains
             | Desert
             deriving (Eq, Ord, Show)

enumTuple :: (Enum a, Enum b) => (a, b) -> (a, b) -> [(a, b)]
enumTuple (a, b) (c, d) = [(x, y) | x <- [a..c], y <- [b..d]]

type TileDistribution = Map TerrainGroup (Map Terrain Int)

fromList' :: (MonadRandom m, Integral t) => [(a, t)] -> m a
fromList' = fromList . map (second fromIntegral)

selectTile :: (MonadRandom m, MonadState TileDistribution m) => TerrainGroup -> m Terrain
selectTile group = do dist <- get
                      tile <- fromList' . M.toList $ (dist ! group)
                      put $ M.adjust (\x -> M.adjust pred tile x) group dist
                      return tile

mkBoardSpace :: (MonadRandom m, MonadState TileDistribution m) => TerrainGroup -> m BoardSpace
mkBoardSpace group = do tile <- selectTile group
                        return $ BoardSpace { terrainGroup = group
                                            , discovered = False
                                            , terrain = tile
                                            }

mkBoard :: (MonadRandom m) => GameMap -> TileDistribution -> m GameBoard
mkBoard template dist = evalStateT (T.mapM mkBoardSpace template) dist

defaultDistribution :: TileDistribution
defaultDistribution = M.fromList [(A, tenEach)]
    where tenEach = M.fromList [(Plains, 10), (Hills, 10), (Mountains, 10), (Desert, 10)]

uniformMap :: (Int, Int) -> GameMap
uniformMap dims = M.fromList $ zip (enumTuple (0,0) dims) (repeat A)

adjacentLocations :: BoardLocation -> GameBoard -> [BoardLocation]
adjacentLocations (x, y) board = filter (flip M.member board) 
                                        [(x - 1, y - 1) -- northwest
                                        ,(x, y - 1) -- north
                                        ,(x + 1, y - 1) --  northeast
                                        ,(x - 1, y) -- southwest
                                        ,(x, y + 1) -- south
                                        ,(x + 1, y) -- southeast
                                        ]