module DivisionOfLabor.Worker where

import qualified Data.Vector as V
import Data.Vector (Vector)
import DivisionOfLabor.Board
import DivisionOfLabor.Resource

data Worker = Worker
    { location :: BoardLocation
    , cargo :: Vector (Resource, Int)
    , upgrades :: [Upgrade]
    } deriving (Eq, Show)

type Upgrade = Int