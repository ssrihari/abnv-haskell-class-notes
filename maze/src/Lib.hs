module Lib
    ( someFunc
    ) where

type Coord = (Int, Int)

data Wall = Open | Closed deriving (Show, Eq)

data Node = Node {  nodeCoord :: Coord
                  ,    nodeUp :: Wall
		  ,  nodeDown :: Wall
		  ,  nodeLeft :: Wall
		  , nodeRight :: Wall } deriving (Show, Eq)

data Maze = Maze { mazeWidth  :: Int
                 , mazeHeight :: Int
		 , mazeNodes  :: Map Coord (Set Dir) } deriving (Show, Eq)

initMaze :: Int -> Int -> Maze
initMaze w h = Maze w h [Node (x,y) Closed Closed Closed Closed | x <- [1..w], y <- [1..h]]

-- renderNode ::
renderMaze :: Maze -> String
renderMaze (Maze w h mazeNodes) =
 concat [renderRow (getRow i) | i <- [1..h] ]
  where
    renderNode n = ("+---+", "|   |", "+---+")
    getRow rowIndex = take w . drop (w * (rowIndex - 1)) $ mazeNodes
    renderRow row =
      let (fa, fb, fc) =
               foldl (\(ra, rb, rc) (a, b, c) -> (ra ++ a, rb ++ b, rc ++ c)) ("", "", "")
               . map renderNode
               $ row
      in unlines [fa, fb, fc]

-- +---+---+---+
-- |           |
-- +---+---+   +
-- |           |
-- +---+   +---+
-- |           |
-- +---+---+---+


someFunc = undefined
