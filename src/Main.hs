module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import System.IO.Error (catchIOError)

main :: IO ()
main = do
  highScores <- loadHighScores
  let initState = initialState { gsHighScores = highScores }
  playIO 
    (InWindow "Asteroids - Wave Survival" (round screenWidth, round screenHeight) (100, 100))
    black
    60  -- 60 FPS for smooth gameplay
    initState
    view
    input
    step

-- | Load high scores from file
loadHighScores :: IO [HighScore]
loadHighScores = do
  content <- catchIOError (readFile "src/highscores.txt") (const $ return "[]")
  return $ case reads content of
    [(scores, "")] -> scores
    _ -> []