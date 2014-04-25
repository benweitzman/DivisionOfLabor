module DivisionOfLabor.Player where

import DivisionOfLabor.Worker

type PlayerId = Int

data Player = Player
    { name :: String
    , playerId :: PlayerId
    , workers :: [Worker]
    , money :: Int
    , role :: Role
    } deriving (Eq, Show)

data Role = Explorer
          | Farmer
          | Manufacturer deriving (Eq, Show)

startingWorkers :: Role -> [Worker]
startingWorkers Explorer = [scout, scout, settler]
startingWorkers Farmer = [standard, standard]
startingWorkers Manufacturer = [standard, standard]

mkPlayer :: String -> Role -> PlayerId -> Player
mkPlayer name role pid = Player { name = name
                                , playerId = pid
                                , money = 2000
                                , role = role
                                , workers = startingWorkers role
                                }