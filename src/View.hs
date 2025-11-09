module View where

import Graphics.Gloss
import Model

-- | Main view function
view :: GameState -> IO Picture
view = return . viewPure

-- | Pure rendering function
viewPure :: GameState -> Picture
viewPure gstate = case gsGameMode gstate of
  Menu -> renderMenu gstate
  EnteringName -> renderNameEntry gstate
  GameOverScreen -> renderGameOver gstate
  Playing
    | gsPaused gstate -> pictures
        [ renderGame gstate
        , color (makeColor 0 0 0 0.5) $ rectangleSolid screenWidth screenHeight
        , color yellow $ translate (-100) 0 $ scale 0.3 0.3 $ text "PAUSED"
        , color white $ translate (-150) (-50) $ scale 0.2 0.2 $ text "Press P to Resume"
        ]
    | otherwise -> renderGame gstate

-- | Render the active game
renderGame :: GameState -> Picture
renderGame gstate = pictures
  [ renderBackground
  , renderPlayer (gsPlayer gstate)
  , renderBullets (gsBullets gstate)
  , renderEnemies (gsEnemies gstate)
  , renderAsteroids (gsAsteroids gstate)
  , renderPickups (gsPickups gstate)
  , renderHUD gstate
  ]

-- | Render main menu
renderMenu :: GameState -> Picture
renderMenu gstate = pictures
  [ renderBackground
  , color cyan $ translate (-250) 150 $ scale 0.8 0.8 $ text "ASTEROIDS"
  , color white $ translate (-180) 80 $ scale 0.3 0.3 $ text "Wave Survival"
  , color yellow $ translate (-220) (-20) $ scale 0.25 0.25 $ text "Press SPACE to Start"
  , renderControls' 0
  , renderHighScoresMenu (gsHighScores gstate)
  ]

-- | Render controls on menu
renderControls' :: Float -> Picture
renderControls' yOffset = translate (-150) (yOffset - 100) $ pictures
  [ color white $ scale 0.2 0.2 $ text "CONTROLS:"
  , translate 0 (-30) $ color (greyN 0.8) $ scale 0.15 0.15 $ text "Arrow Keys or A/D - Move"
  , translate 0 (-50) $ color (greyN 0.8) $ scale 0.15 0.15 $ text "Space - Shoot"
  , translate 0 (-70) $ color (greyN 0.8) $ scale 0.15 0.15 $ text "P - Pause"
  ]

-- | Render high scores on menu
renderHighScoresMenu :: [HighScore] -> Picture
renderHighScoresMenu scores = translate (150) 100 $ pictures $
  [ color yellow $ scale 0.2 0.2 $ text "TOP SCORES" ] ++
  [ translate 0 (fromIntegral (-i) * 25 - 30) $ 
    color white $ scale 0.12 0.12 $ 
    text $ show (i+1) ++ ". " ++ hsName score ++ " - " ++ show (hsScore score)
  | (i, score) <- zip [0..] (take 10 scores)
  ]

-- | Render game over screen
renderGameOver :: GameState -> Picture
renderGameOver gstate = pictures
  [ renderBackground
  , color red $ translate (-180) 100 $ scale 0.5 0.5 $ text "GAME OVER"
  , color white $ translate (-100) 40 $ scale 0.3 0.3 $ text $ "Score: " ++ show (gsScore gstate)
  , color white $ translate (-130) 0 $ scale 0.3 0.3 $ text $ "Wave: " ++ show (waveNumber $ gsWave gstate)
  , renderGameOverOptions gstate
  ]

-- | Render game over options
renderGameOverOptions :: GameState -> Picture
renderGameOverOptions gstate = translate 0 (-80) $ pictures $
  if qualifiesForHighScore' (gsScore gstate) (gsHighScores gstate)
    then [ color green $ translate (-180) 0 $ scale 0.18 0.18 $ text "NEW HIGH SCORE!"
         , color yellow $ translate (-150) (-30) $ scale 0.15 0.15 $ text "Press S to Save Score"
         , color white $ translate (-120) (-60) $ scale 0.15 0.15 $ text "Press R to Retry"
         , color white $ translate (-120) (-80) $ scale 0.15 0.15 $ text "Press M for Menu"
         ]
    else [ color white $ translate (-120) 0 $ scale 0.15 0.15 $ text "Press R to Retry"
         , color white $ translate (-120) (-20) $ scale 0.15 0.15 $ text "Press M for Menu"
         ]

qualifiesForHighScore' :: Int -> [HighScore] -> Bool
qualifiesForHighScore' score highScores
  | length highScores < 10 = True
  | otherwise = score > minimum (map hsScore highScores)

-- | Render name entry screen
renderNameEntry :: GameState -> Picture
renderNameEntry gstate = pictures
  [ renderBackground
  , color yellow $ translate (-200) 100 $ scale 0.4 0.4 $ text "ENTER NAME"
  , color white $ translate (-150) 30 $ scale 0.2 0.2 $ text $ "Score: " ++ show (gsScore gstate)
  , color cyan $ translate (-100) (-30) $ scale 0.25 0.25 $ text $ gsPlayerName gstate ++ "_"
  , color white $ translate (-180) (-100) $ scale 0.15 0.15 $ text "Press ENTER to save"
  , color white $ translate (-200) (-120) $ scale 0.15 0.15 $ text "Press DELETE to delete"
  ]

-- | Render background
renderBackground :: Picture
renderBackground = color black $ rectangleSolid screenWidth screenHeight

-- | Render player
renderPlayer :: Player -> Picture
renderPlayer player = pictures
  [ translate x y $ color playerColor $ renderShip
  , renderHealthBar player
  ]
  where
    (x, y) = pPos player
    hasInstaKill = any (\p -> case apType p of InstaKill _ -> True; _ -> False) (pPowerups player)
    playerColor = if hasInstaKill then red else cyan
    
    renderShip = pictures
      [ polygon [(-15, -20), (0, 20), (15, -20)]
      , line [(-10, -15), (-10, -20)]
      , line [(10, -15), (10, -20)]
      ]

-- | Render health bar
renderHealthBar :: Player -> Picture
renderHealthBar player = translate x (y + 35) $ pictures
  [ color red $ rectangleSolid 40 5
  , color green $ rectangleSolid (40 * healthRatio) 5
  ]
  where
    (x, y) = pPos player
    healthRatio = fromIntegral (pHealth player) / fromIntegral (pMaxHealth player)

-- | Render all bullets
renderBullets :: [Bullet] -> Picture
renderBullets = pictures . map renderBullet

renderBullet :: Bullet -> Picture
renderBullet bullet = translate x y $ color bulletColor $ circleSolid 3
  where
    (x, y) = bPos bullet
    bulletColor = case bOwner bullet of
      FromPlayer -> yellow
      _ -> red

-- | Render all enemies
renderEnemies :: [Enemy] -> Picture
renderEnemies = pictures . map renderEnemy

renderEnemy :: Enemy -> Picture
renderEnemy enemy = translate x y $ color enemyColor $ renderEnemyShape (eType enemy)
  where
    (x, y) = ePos enemy
    enemyColor = case eType enemy of
      Alien -> green
      Martian -> orange
      UFO -> magenta

renderEnemyShape :: EnemyType -> Picture
renderEnemyShape Alien = pictures
  [ polygon [(-10, 10), (10, 10), (15, 0), (10, -10), (-10, -10), (-15, 0)]
  , circle 5
  ]
renderEnemyShape Martian = pictures
  [ polygon [(-15, 0), (0, 15), (15, 0), (0, -15)]
  , line [(-10, 0), (10, 0)]
  , line [(0, -10), (0, 10)]
  ]
renderEnemyShape UFO = pictures
  [ arc 0 180 20
  , arc 180 360 20
  , line [(-20, 0), (20, 0)]
  , circle 5
  ]

-- | Render all asteroids
renderAsteroids :: [Asteroid] -> Picture
renderAsteroids = pictures . map renderAsteroid

renderAsteroid :: Asteroid -> Picture
renderAsteroid asteroid = translate x y $ color (greyN 0.5) $ renderAsteroidShape (aSize asteroid)
  where
    (x, y) = aPos asteroid

renderAsteroidShape :: AsteroidSize -> Picture
renderAsteroidShape size = lineLoop points
  where
    radius = case size of
      Large -> 40
      Medium -> 25
      Small -> 15
    points = [(radius * cos a, radius * sin a) | a <- [0, pi/4 .. 2*pi]]

-- | Render all pickups
renderPickups :: [Pickup] -> Picture
renderPickups = pictures . map renderPickup

renderPickup :: Pickup -> Picture
renderPickup pickup = translate x y $ color pickupColor $ renderPickupShape
  where
    (x, y) = puPos pickup
    pickupColor = case puType pickup of
      HealthDrop _ -> green
      InstaKill _ -> red
      Nuke -> orange
      ScoreStar _ -> yellow
    
    renderPickupShape = pictures
      [ circleSolid 10
      , color black $ circle 10
      , renderPickupIcon (puType pickup)
      ]

renderPickupIcon :: PickupType -> Picture
renderPickupIcon (HealthDrop _) = color white $ scale 0.1 0.1 $ text "H"
renderPickupIcon (InstaKill _) = color white $ scale 0.1 0.1 $ text "I"
renderPickupIcon Nuke = color white $ scale 0.1 0.1 $ text "N"
renderPickupIcon (ScoreStar _) = color white $ scale 0.1 0.1 $ text "S"

-- | Render HUD (score, wave, etc.)
renderHUD :: GameState -> Picture
renderHUD gstate = pictures
  [ translate (-screenWidth/2 + 20) (screenHeight/2 - 30) $ color white $ scale 0.15 0.15 $ text ("Score: " ++ show (gsScore gstate))
  , translate (-screenWidth/2 + 20) (screenHeight/2 - 50) $ color white $ scale 0.15 0.15 $ text ("Wave: " ++ show (waveNumber wave))
  , translate (-screenWidth/2 + 20) (screenHeight/2 - 70) $ color white $ scale 0.15 0.15 $ text ("Kills: " ++ show (killsThisWave wave) ++ "/" ++ show (killsRequired wave))
  , translate (-screenWidth/2 + 20) (screenHeight/2 - 90) $ color white $ scale 0.15 0.15 $ text ("Health: " ++ show (pHealth player))
  , renderPowerupIndicators (pPowerups player)
  ]
  where
    wave = gsWave gstate
    player = gsPlayer gstate

-- | Render active powerup indicators
renderPowerupIndicators :: [ActivePowerup] -> Picture
renderPowerupIndicators powerups = translate (-screenWidth/2 + 20) (screenHeight/2 - 110) $ 
  pictures [renderPowerupIndicator i p | (i, p) <- zip [0..] powerups]

renderPowerupIndicator :: Int -> ActivePowerup -> Picture
renderPowerupIndicator idx powerup = translate 0 (fromIntegral idx * (-20)) $ color powerupColor $ scale 0.12 0.12 $ text powerupText
  where
    (powerupColor, powerupText) = case apType powerup of
      InstaKill _ -> (red, "INSTA-KILL: " ++ show (round (apRemaining powerup)) ++ "s")
      _ -> (white, "Powerup")