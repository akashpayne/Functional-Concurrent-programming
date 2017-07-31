------------------------------------------------------------------
--								--
--	Maze.hs							--
--	Simon Thompson						--
--	March 2001						--
--								--
------------------------------------------------------------------

--	A module solving mazes.

module Maze where
import Test.QuickCheck

--	Type of Mazes
-- 	It is globally assumed that mazes are rectangular,
--	so that all the lines are of the same length.

type Maze = [[Bool]]

--	Mazes as Strings

mazeSt1 :: [String]

mazeSt1 
  = ["..#..####",
     "#...#...#",
     "..#...#.#",
     "##.##.#.#",
     "....#...#",
     "#.#...#.#",
     "#.##.##.#",
     "#..#....#",
     "##...##.#",
     "..#..##.."]

-- Maze coordinates measured from top left hand corner, starting from 0.
-- For example, (4,2) is shown by X:
--
--    ..#######
--    #...#...#
--    ###...#.#
--    ##.##.###
--    #.X.#...#
--    #.#...#.#
--    #.##.####
--    #..#....#
--    ##...##.#
--    #######..

makeMaze :: [String] -> Maze
makeMaze = map (map (=='.'))

maze1 :: Maze
maze1 = makeMaze mazeSt1

maze0 = top ++ top 
        where 
        top = zipWith (++) maze1 maze1

-- A (potential) path is given by a list of points.

type Point = (Int,Int)
type Path = [Point]

path1, path2 :: Path

path1 = [ (0,0), (0,1), (1,1), (1,2), (1,3), (2,3), (2,4), (2,5),
          (3,5), (4,5), (5,5), (5,4), (5,3), (4,3), (4,2), (4,1),
          (5,1), (6,1), (7,1), (7,2), (8,2), (8,3), (8,4), (7,4),         
          (7,5), (7,6), (7,7), (8,7), (9,7), (9,8)]

path2 = [ (0,0), (0,1), (1,1), (1,2), (1,3), (2,3), (2,4), (2,5),
          (3,5), (4,5), (5,5), (5,4), (5,3), (5,2), (4,2), (4,1),
          (5,1), (6,1), (7,1), (7,2), (8,2), (8,3), (8,4), (7,4),         
          (7,5), (7,6), (7,7), (8,7), (9,7), (9,8)]


-- Is it a path through the maze?

isPath :: Maze -> Path -> Bool

isPath = isPath

-- Does a point lie in the rectangular area of the maze?

inGrid :: Maze -> Point -> Bool

inGrid maze (n,m) = 
  0 <= n && n< length maze &&
  0 <= m && m < length (head maze)

-- Is a point (assumed to be within the grid) empty in the maze?

isEmpty :: Maze -> Point -> Bool

isEmpty maze (n,m) = maze!!n!!m

-- Are two points adjacent?

adjacent :: Point -> Point -> Bool

adjacent (a,b) (c,d) = abs(a-c) + abs(b-d) == 1

-- All the points adjacent to a given point.

adjPoints :: Maze -> Point -> [Point]

adjPoints maze (n,m) =
  [ (n+p,m+q) | p<-[1,0,-1], q<-[1,0,-1], adjacent (n,m) (n+p,m+q), inGrid maze (n+p,m+q), isEmpty maze (n+p,m+q) ] 
        

-- List of all paths from A to B.

paths :: Maze -> Point -> Point -> [Path]

paths maze p q  
  | isEmpty maze p && (p==q) = [[p]]
  | isEmpty maze p =
     [ p:path | z <- adjPoints maze p , path <- paths maze z q ]  

pathsNC :: Maze -> Point -> Point -> [Point] -> [Path]

pathsNC maze p q avoid
  | isEmpty maze p && (p==q) = [[p]]
  | isEmpty maze p =
     [ p:path | z <- adjPoints maze p , not (elem z avoid), path <- pathsNC maze z q (p:avoid) ]  









--
-- Properties
--

prop_path :: Maze -> Int -> Int -> Int -> Int -> Property

prop_path maze x y z w =
        (inGrid maze (x,y) && inGrid maze (z,w) ) ==> all (isPath maze) (paths maze (x,y) (z,w))

prop_wrong' :: Maze -> Int -> Int -> Property

prop_wrong' maze x y =
        inGrid maze (x,y)  ==> elem [(x,y)] $ paths maze (x,y) (x,y)

prop_wrong :: Maze -> Int -> Int -> Bool

prop_wrong maze x y = 
        elem [(x',y')] $ paths maze (x',y') (x',y')
        where
        x' = x `mod` (length maze)
        y' = y `mod` (length $ head maze)


