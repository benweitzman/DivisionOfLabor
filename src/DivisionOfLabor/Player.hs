module DivisionOfLabor.Player where

import DivisionOfLabor.Worker

type PlayerId = Int

data Player = Player
    { name :: String
    , playerId :: PlayerId
    , workers :: [Worker]
    , money :: Int
    } deriving (Eq, Show)