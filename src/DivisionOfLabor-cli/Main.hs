{-# LANGUAGE OverloadedStrings #-}

module Main where

import DivisionOfLabor.Game
import DivisionOfLabor.Player
import DivisionOfLabor.Board
import DivisionOfLabor.Graphics

import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.HexGrid

import qualified Data.Text as T
import Data.Monoid

import System.Exit ( exitSuccess )

main = do
    gameState <- mkGameState [mkPlayer "Ben" Explorer, mkPlayer "Alex" Farmer, mkPlayer "Charles" Manufacturer] (uniformMap (5, 5))

    mapWidget <- newHexGrid (board gameState) tilePrinter
    playersWidget <- newTextList (bgColor blue)$ map (T.pack . name) (getPlayers gameState)
    mainBox <- hBox mapWidget playersWidget >>= withBoxSpacing 1
    ui <- centered =<< hLimit 120 mainBox
    fg <- newFocusGroup

    fg <- newFocusGroup
    fg `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitSuccess
           _ -> return False

    addToFocusGroup fg mapWidget
    addToFocusGroup fg playersWidget

    c <- newCollection
    _ <- addToCollection c ui fg
    runUi c $ defaultContext
    return ()
