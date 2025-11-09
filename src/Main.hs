module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO 
  (InWindow "Asteroids - Wave Survival" (round screenWidth, round screenHeight) (100, 100))
  black
  60  -- 60 FPS for smooth gameplay
  initialState
  view
  input
  step