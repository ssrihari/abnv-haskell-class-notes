module Lib
    ( someFunc
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Here is a maze.
-- +---+---+---+
-- |           |
-- +---+---+   +
-- |           |
-- +---+   +---+
-- |           |
-- +---+---+---+

type Coord = (Int, Int)

data Dir = L | U deriving (Show, Eq, Ord)

type OpenGate = (Coord, Dir)

data Wall = Open | Closed deriving (Show, Eq, Ord)

data Maze = Maze { mazeWidth :: Int
                 , mazeHeight :: Int
                 , openMazeGates :: Set.Set OpenGate } deriving (Show, Eq)

initMaze :: Int -> Int -> Maze
initMaze w h = Maze w h Set.empty

renderMaze :: Maze -> String
renderMaze (Maze w h omg) =
  unlines [renderRow (getRow i) i | i <- [0..h-1]]
  where
    renderFloor _ = "+---"
    renderCeiling c  = if Set.member (c,U) omg then "+   " else "+---"
    renderLeftWall c = if Set.member (c,U) omg then "   " else "|   "
    renderNormalRow row = (concatMap renderCeiling row)
                          ++ "+\n"
			  ++ (concatMap renderLeftWall row)
			  ++ "|"

    renderLastRow row   = (concatMap renderCeiling row)
                          ++ "+\n"
			  ++ (concatMap renderLeftWall row)
			  ++ "|\n"
                          ++ (concatMap renderFloor row)
			  ++ "+"

    getRow rowIndex = [(rowIndex, y) | y <- [0..w-1]]
    renderRow :: [Coord] -> Int -> String
    renderRow row i = if i /= h-1 then renderNormalRow row
                      else renderLastRow row


createMaze :: Int -> Int -> Maze
createMaze w h =
  Maze w h $ foldl walk Set.empty allCoords
  where
    allCoords = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    walk openGates (x,y) =

someFunc = undefined
