module MarsRover where

import Control.Arrow

data Direction = North | South | East | West deriving (Show, Eq)
data Position = Position Int Int deriving (Show, Eq)
data Rover = Rover Direction Position deriving (Show, Eq)

leftOf :: Direction -> Direction
leftOf d = case d of
	North -> West
	East -> North
	South -> East
	West -> South

rightOf :: Direction -> Direction
rightOf d = case d of
	North -> East
	East -> South
	South -> West
	West -> North

turnLeft :: Rover -> Rover
turnLeft (Rover d p) = Rover (leftOf d) p

turnRight :: Rover -> Rover
turnRight (Rover d p) = Rover (rightOf d) p

move :: Rover -> Rover
move (Rover d (Position x y)) = case d of
	North -> Rover d (Position x (y + 1))
	East -> Rover d (Position (x + 1) y)
	South -> Rover d (Position x (y - 1))
	West -> Rover d (Position (x - 1) y)

getCommand :: Char -> Rover -> Rover
getCommand instruction = case instruction of
	'L' -> turnLeft 
	'R' -> turnRight
	'M' -> move

parseInstructions :: String -> Rover -> Rover
parseInstructions instructions = foldr (>>>) id (map getCommand instructions)

myRover = Rover West (Position 0 0)
instructions = "RRMMR" --expected outcome is (Rover South Position (2 0))