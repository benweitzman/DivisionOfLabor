module Graphics.Vty.Widgets.HexGrid

where

import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.HexGrid
import Data.Monoid

import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util



type HexTileData = ( Char -- character to use to fill the tile
                   , (String, String) -- (top row, bottom row) for tile info
                   )

data HexGridWidget a = HexGridWidget { hexGrid :: HexGrid a
                                     , selectedLocation :: HexLocation
                                     , tileData :: a -> HexTileData 
                                     }

instance (Show a) => Show (HexGridWidget a) where
    show HexGridWidget { hexGrid = g
                       , selectedLocation = s
                       } =
        "{hexGrid=" ++ show g ++ ", selectedLocation=" ++ show s ++ "}"

newHexGridData :: HexGrid a -> (a -> HexTileData) -> HexGridWidget a
newHexGridData grid tileRenderer = HexGridWidget { hexGrid = grid
                                                 , selectedLocation = minQ grid
                                                 , tileData = tileRenderer
                                                 }

newHexGrid :: HexGrid a -> (a -> HexTileData) -> IO (Widget (HexGridWidget a))
newHexGrid g tileRender = do
  let grid = newHexGridData g tileRender
  wRef <- newWidget grid $ \w ->
      w { getCursorPosition_ = const $ return Nothing
        , render_ =
            \this sz ctx -> do

              hexGridData <- getState this
              foc <- focused <~ this

              renderHexGrid foc hexGridData sz ctx
        , keyEventHandler = hexGridKeyEvent
        }

  return wRef

renderHexGrid :: Bool -> HexGridWidget a -> DisplayRegion -> RenderContext -> IO Image
renderHexGrid foc grid s ctx = do
    let gridString = prettyPrint undefined (hexGrid grid) (selectedLocation grid)
        rows = lines gridString
        displayStringRows = map (\x -> S.fromList $ map (\c -> (c, 1)) x) rows
        width = fromIntegral $ S.length (displayStringRows !! 0)
        images = map (\x -> HorizText mempty x width 1) displayStringRows
    return $ vert_cat images

hexGridKeyEvent :: Widget (HexGridWidget a) -> Key -> [Modifier] -> IO Bool
hexGridKeyEvent w KUp _ = moveUp w >> return True
hexGridKeyEvent w KDown _ = moveDown w >> return True
hexGridKeyEvent w KLeft _ = moveLeft w >> return True
hexGridKeyEvent w KRight _ = moveRight w >> return True
hexGridKeyEvent _ _ _ = return False

moveByDirection :: Direction -> Widget (HexGridWidget a) -> IO ()
moveByDirection dir wRef = do
    HexGridWidget{ selectedLocation = s
                 , hexGrid = grid
                 } <- state <~ wRef
    when (M.member (dir `ofLocation` s) grid) $
        updateWidgetState wRef $ (\h@HexGridWidget{selectedLocation=s} -> h{selectedLocation=dir `ofLocation` s})

moveUp :: Widget (HexGridWidget a) -> IO ()
moveUp = moveByDirection NorthWest

moveDown :: Widget (HexGridWidget a) -> IO ()
moveDown = moveByDirection SouthEast

moveLeft :: Widget (HexGridWidget a) -> IO ()
moveLeft = moveByDirection West

moveRight :: Widget (HexGridWidget a) -> IO ()
moveRight = moveByDirection East

----------------------------
-- Pure Hexgrid Rendering --
----------------------------

hexWidth :: Int
hexWidth = 9

hexHeight :: Num a => a
hexHeight = 6

toXY :: HexLocation -> (Int, Int)
toXY (q, r) = (8 * q + 4 * r, 4 * r)

type CharGrid = Vector (Vector Char)

update2' :: CharGrid -> ((Int, Int), Char) -> CharGrid
update2' grid ((y, x), c) = grid V.// [(y, grid V.! y V.// [(x, c)])]

update2 :: CharGrid -> [((Int, Int), Char)]  -> CharGrid
update2 = foldl update2'

prettyPrint :: (a -> String) -> HexGrid a -> HexLocation -> String                                              
prettyPrint f grid selected = toString $ M.foldWithKey printHex blankCharGrid grid
    where printHex :: HexLocation -> a -> CharGrid -> CharGrid
          printHex p _ c = c `update2` ([((hexY,hexX+3), '/')
                                       ,((hexY,hexX+5), '\\')
                                       ,((hexY+1,hexX+1), '/')
                                       ,((hexY+1,hexX+7), '\\')
                                       ,((hexY+2,hexX), '|')
                                       ,((hexY+2,hexX+8), '|')
                                       ,((hexY+3,hexX), '|')
                                       ,((hexY+3,hexX+8), '|')
                                       ,((hexY+4,hexX+1), '\\')
                                       ,((hexY+4,hexX+7), '/')
                                       ,((hexY+5,hexX+3), '\\')
                                       ,((hexY+5,hexX+5), '/')
                                       ] ++ selectedUpdates) -- ++ map (\((y, x), c) -> ((y+hexY, x+hexX), c)) (makeFill f (grid ! p)))
            where (hexX, hexY) = toXY p
                  selectedUpdates = if p == selected 
                                    then [((hexY, hexX+4), '*')
                                         ,((hexY+1,hexX+2), '*')
                                         ,((hexY+1,hexX+6), '*')
                                         ,((hexY+2,hexX+1), '*')
                                         ,((hexY+2,hexX+7), '*')
                                         ,((hexY+3,hexX+1), '*')
                                         ,((hexY+3,hexX+7), '*')
                                         ,((hexY+4,hexX+2), '*')
                                         ,((hexY+4,hexX+6), '*')
                                         ,((hexY+5,hexX+4), '*')
                                         ] 
                                    else []
          blankCharGrid :: CharGrid
          blankCharGrid = V.replicate gridHeight (V.replicate gridWidth ' ')
          gridHeight = maxY - minY + hexHeight
          gridWidth = maxX - minX + hexWidth
          minX = min (fst $ toXY qMin) (fst $ toXY rMin)
          minY = min (snd $ toXY qMin) (snd $ toXY rMin)
          maxX = max (fst $ toXY qMax) (fst $ toXY rMax)
          maxY = max (snd $ toXY qMax) (snd $ toXY rMax)
          qMax = maxQ grid
          qMin = minQ grid
          rMax = maxR grid
          rMin = minR grid
          toString :: CharGrid -> String
          toString = concat . V.toList . V.map (\x -> V.toList x ++ "\n")

{-



  The 'prettyPrint' function will print a hex grid
  using a supplied function to generate the inside of each
  hexagon

  The real estate that the supplied function has to work with
  is as shown:

@

|---9---|  ---
   \/*\\      |
 \/*****\\    |
|*******|   6
|*******|   |
 \\*****\/    |
   \\*\/     _|_
@

  That's

@
  1
  5
  7
  7
  5
  1
@

  characters per row, respectively

makeFill :: (a -> String) -> a -> [((Int, Int), Char)]
makeFill f x = concat updateRows
    where updateRows = map (\((takeN, dropN), rowNum) -> take takeN . drop dropN . zipWith (\x (y, c) -> ((y, x), c)) [0..] . map (\c -> (rowNum, c)) $ rows !! rowNum)
                           (zip rowData [0..])
          rows = lines $ f x
          rowData = [(1, 4), (5, 2), (7, 1), (7, 1), (5, 2), (1, 4)]

-}