module DivisionOfLabor.Worker where

import qualified Data.Vector as V
import Data.Vector (Vector)
import DivisionOfLabor.Board
import DivisionOfLabor.Resource

data Worker = Worker
    { location :: Maybe BoardLocation
    , cargo :: Vector (Maybe (Resource, Int))
    , upgrades :: [Upgrade]
    , speed :: Int
    } deriving (Eq, Show)

type Upgrade = Int

scout :: Worker
scout = undefined

settler :: Worker
settler = undefined

standard :: Worker
standard = undefined