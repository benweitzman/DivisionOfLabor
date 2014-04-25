module DivisionOfLabor.Graphics where

import DivisionOfLabor.Board

import Graphics.Vty.Widgets.HexGrid

tilePrinter :: BoardSpace -> HexTileData
tilePrinter BoardSpace{discovered=False,terrainGroup=g}= ('?', (" " ++ show g ++ " ", "   "))
