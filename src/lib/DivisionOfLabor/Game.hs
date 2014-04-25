{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module DivisionOfLabor.Game where

import qualified Data.Map as M
import Data.Map (Map)

import Control.Applicative
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Logger
import Control.Monad.Error

import DivisionOfLabor.Player
import DivisionOfLabor.Board

data GameState = GameState
    { players :: Map PlayerId Player
    , board :: GameBoard
    , roundNum :: Int
    } deriving (Eq, Show)

newtype TransStackT e s m a = DivisionOfLabor { runDivision :: ErrorT e (StateT s m) a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadRandom, MonadIO, MonadLogger, MonadError e)

type DivisonOfLabor a = forall m . TransStackT String GameState m a

mkGameState :: [(PlayerId -> Player)] -> GameMap -> IO GameState
mkGameState playerFs map = do b <- evalRandIO (mkBoard map defaultDistribution)
                              return GameState { players = M.fromList $ mkPlayers playerFs
                                               , board = b
                                               , roundNum = 0
                                               }
    where mkPlayers = zipWith (\x f -> (x, f x)) [0..]

getPlayers :: GameState -> [Player]
getPlayers = M.elems . players
