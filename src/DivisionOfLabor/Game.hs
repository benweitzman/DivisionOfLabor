{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DivisionOfLabor.Game where

import qualified Data.Map as M
import Data.Map (Map)

import Control.Monad.State
import Control.Monad.Random

import DivisionOfLabor.Player
import DivisionOfLabor.Board

data GameState = GameState
    { players :: Map PlayerId Player
    , board :: GameBoard
    , roundNum :: Int
    } deriving (Eq, Show)

newtype DivisionOfLabor a = DivisionOfLabor { runDivision :: StateT GameState (RandT StdGen IO) a } 
    deriving (Monad, MonadState GameState, MonadRandom, MonadIO)


mkGameState :: [(PlayerId -> Player)] -> GameMap -> IO (GameState)
mkGameState playerFs map = do b <- evalRandIO (mkBoard map defaultDistribution)
                              return GameState { players = M.fromList $ mkPlayers playerFs 
                                               , board = b
                                               , roundNum = 0
                                               }
    where mkPlayers = zipWith (\x f -> (x, f x)) [0..]
