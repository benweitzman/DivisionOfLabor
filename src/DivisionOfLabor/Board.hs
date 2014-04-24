{-# LANGUAGE FlexibleContexts #-}

module DivisionOfLabor.Board where

import Data.Map ((!))
import qualified Data.Map as M
import Data.Map (Map)    
import qualified Data.Traversable as T
import Data.HexGrid hiding (fromList)
import qualified Data.HexGrid as HG

import Control.Monad.Random
import Control.Monad.State
import Control.Arrow

type BoardLocation = HexLocation

type GameMap = HexGrid TerrainGroup

type GameBoard = HexGrid BoardSpace

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
uniformMap (width, height) = parallelogram width height (const A)
