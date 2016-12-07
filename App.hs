module RoverApp where

import Control.Arrow
import RoverModules

parseDirection :: String -> Direction
parseDirection input = case input of
  "N" -> North
  "S" -> South
  "E" -> East
  "W" -> West
  _ -> undefined

getCommand :: Char -> Rover -> Rover
getCommand instruction = case instruction of
  'L' -> turnLeft 
  'R' -> turnRight
  'M' -> move
  _ -> undefined

getCommandCompositionFromInstructions :: String -> Rover -> Rover
getCommandCompositionFromInstructions instructions = foldr (>>>) id (map getCommand instructions)

main :: IO()
main = do
  putStr "Enter initial direction of the rover (N - North, S - South...): " 
  direction <- parseDirection <$> getLine
  putStr "Enter initial X coordinate of the rover: "
  x <- read <$> getLine
  putStr "Enter initial Y coordinate of the rover: "
  y <- read <$> getLine
  let position = Position x y
  let initialRover = Rover direction position
  putStrLn ("The initial rover is ready: " ++ (show initialRover))
  putStr "Now enter the sequence of instructions: "
  composedCommandSequence <- getCommandCompositionFromInstructions <$> getLine
  let finalRover = composedCommandSequence initialRover
  putStrLn ("The finalRover rover is: " ++ (show finalRover))

test :: IO()
test = do
  let myInitialRover = Rover West (Position 0 0)
  let instructions = "RRMMR" --expected outcome is (Rover South Position (2 0))
  let composedCommandSequence = getCommandCompositionFromInstructions instructions
  let myFinalRover = composedCommandSequence myInitialRover
  putStrLn (show myFinalRover)