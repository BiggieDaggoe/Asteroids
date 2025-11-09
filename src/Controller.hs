module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (partition, (\\))

-- | Main game step function
step :: Float -> GameState -> IO GameState
step secs gstate
  | gsGameMode gstate == Menu = return gstate
  | gsGameOver gstate = return gstate
  | gsPaused gstate && gsGameMode gstate /= EnteringName = return gstate
  | gsGameMode gstate == EnteringName = return gstate  -- Still process input in name entry
  | otherwise = return $ updateGame secs gstate

-- | Update all game systems
updateGame :: Float -> GameState -> GameState
updateGame dt gstate = gstate
  & updatePlayer dt
  & updateBullets dt
  & updateEnemies dt
  & updateAsteroids dt
  & updatePickups dt
  & updateCollisions
  & updateWaveSpawning dt
  & checkWaveCompletion
  & updateTimeAndPowerups dt
  & checkGameOver

-- | Function composition operator for better readability
(&) :: a -> (a -> b) -> b
x & f = f x

-- | Update player position and cooldowns
updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gstate = gstate { gsPlayer = newPlayer }
  where
    player = gsPlayer gstate
    (px, py) = pPos player
    (vx, vy) = pVel player
    
    newX = clamp (-screenWidth/2 + 20) (screenWidth/2 - 20) (px + vx * dt)
    newCooldown = max 0 (pFireCooldown player - dt)
    
    newPlayer = player
      { pPos = (newX, py)
      , pFireCooldown = newCooldown
      }

-- | Update all bullets
updateBullets :: Float -> GameState -> GameState
updateBullets dt gstate = gstate { gsBullets = newBullets }
  where
    bullets = gsBullets gstate
    movedBullets = map (moveBullet dt) bullets
    newBullets = filter isOnScreen movedBullets
    
    moveBullet dt' bullet = bullet
      { bPos = addVec (bPos bullet) (scaleVec dt' (bVel bullet))
      }
    
    isOnScreen bullet = let (_, y) = bPos bullet
                        in y > -screenHeight/2 - 50 && y < screenHeight/2 + 50

-- | Update all enemies
updateEnemies :: Float -> GameState -> GameState
updateEnemies dt gstate = gstate 
  { gsEnemies = remainingEnemies ++ respawnedMartians
  , gsBullets = gsBullets gstate ++ newBullets
  , gsNextId = gsNextId gstate + EntityId (length newBullets)
  }
  where
    enemies = gsEnemies gstate
    movedEnemies = map (moveEnemy dt) enemies
    
    -- Split enemies into those on screen and those that escaped
    (remainingEnemies, escapedEnemies) = partition isOnScreen movedEnemies
    
    -- Respawn escaped Martians at the top of the screen
    respawnedMartians = map respawnEnemy $ filter (\e -> eType e == Martian) escapedEnemies
    
    -- Generate UFO bullets aimed at player
    playerPos = pPos $ gsPlayer gstate
    nextId = gsNextId gstate
    newBullets = concat [maybeShootUFO e (nextId + EntityId idx) playerPos | (e, idx) <- zip remainingEnemies [0..]]
    
    moveEnemy dt' enemy = enemy
      { ePos = addVec (ePos enemy) (scaleVec dt' (eVel enemy))
      }
    
    respawnEnemy enemy = enemy
      { ePos = (fst (ePos enemy), screenHeight/2 + 30)  -- Keep same X position, move to top
      }
    
    isOnScreen enemy = let (_, y) = ePos enemy
                      in y > -screenHeight/2 - 50
    
    -- UFO shooting logic
    maybeShootUFO enemy bulletId targetPos
      | eType enemy /= UFO = []  -- Only UFOs shoot
      | otherwise = 
          let chance = sin (gsTimeElapsed gstate * 2) * 0.5 + 0.5  -- Smooth shooting pattern
              shootThisFrame = chance > 0.95  -- ~5% chance to shoot each frame
              (ex, ey) = ePos enemy
              (px, py) = targetPos
              -- Calculate direction vector to player
              dx = px - ex
              dy = py - ey
              -- Normalize the vector and scale by bullet speed
              len = sqrt (dx * dx + dy * dy)
              bulletVel = if len == 0 
                         then (0, -200)  -- Default downward if directly above/below
                         else (dx / len * 200, dy / len * 200)
          in if shootThisFrame 
             then [Bullet
                    { bId = bulletId
                    , bPos = ePos enemy
                    , bVel = bulletVel
                    , bOwner = FromEnemy (eId enemy)  -- Use enemy's ID
                    , bDamage = 1
                    }]
             else []

-- | Update all asteroids
updateAsteroids :: Float -> GameState -> GameState
updateAsteroids dt gstate = gstate { gsAsteroids = newAsteroids }
  where
    asteroids = gsAsteroids gstate
    movedAsteroids = map (moveAsteroid dt) asteroids
    newAsteroids = filter isOnScreen movedAsteroids
    
    moveAsteroid dt' asteroid = asteroid
      { aPos = wrapHorizontal (addVec (aPos asteroid) (scaleVec dt' (aVel asteroid)))
      }
    
    isOnScreen asteroid = let (_, y) = aPos asteroid
                         in y > -screenHeight/2 - 50

-- | Update pickups (move and check TTL)
updatePickups :: Float -> GameState -> GameState
updatePickups dt gstate = gstate { gsPickups = newPickups }
  where
    pickups = gsPickups gstate
    movedPickups = map (movePickup dt) pickups
    newPickups = filter (not . isExpired) movedPickups
    
    movePickup dt' pickup = pickup
      { puPos = addVec (puPos pickup) (scaleVec dt' (puVel pickup))
      , puTTL = fmap (\ttl -> ttl - dt') (puTTL pickup)
      }
    
    isExpired pickup = case puTTL pickup of
      Just ttl -> ttl <= 0
      Nothing -> False

-- | Handle all collision detection
updateCollisions :: GameState -> GameState
updateCollisions gstate = gstate
  & handleBulletEnemyCollisions
  & handleBulletAsteroidCollisions
  & handlePlayerPickupCollisions
  & handleEnemyPlayerCollisions
  & handleAsteroidPlayerCollisions
  & handleEnemyBulletPlayerCollisions  -- Add enemy bullet collision handling

-- | Handle enemy bullet collisions with player
handleEnemyBulletPlayerCollisions :: GameState -> GameState
handleEnemyBulletPlayerCollisions gstate = gstate
  { gsPlayer = newPlayer
  , gsBullets = survivingBullets
  }
  where
    player = gsPlayer gstate
    (enemyBullets, otherBullets) = partition isEnemyBullet (gsBullets gstate)
    (hittingBullets, survivingEnemyBullets) = partition (collidesWithPlayer player) enemyBullets
    survivingBullets = otherBullets ++ survivingEnemyBullets  -- Keep non-hitting enemy bullets
    
    damage = sum [bDamage bullet | bullet <- hittingBullets]
    newPlayer = player { pHealth = pHealth player - damage }
    
    isEnemyBullet bullet = case bOwner bullet of
      FromEnemy _ -> True
      _ -> False
    
    collidesWithPlayer p bullet = distance (pPos p) (bPos bullet) < 20  -- Bullet hit radius

-- | Bullet-Enemy collisions
handleBulletEnemyCollisions :: GameState -> GameState
handleBulletEnemyCollisions gstate = gstate
  { gsBullets = survivingBullets
  , gsEnemies = survivingEnemies ++ newEnemies
  , gsPickups = gsPickups gstate ++ newPickups
  , gsScore = gsScore gstate + scoreGained
  , gsWave = wave { killsThisWave = killsThisWave wave + enemiesKilled }
  , gsRandGen = finalGen
  }
  where
    wave = gsWave gstate
    playerBullets = filter (\b -> bOwner b == FromPlayer) (gsBullets gstate)
    otherBullets = filter (\b -> bOwner b /= FromPlayer) (gsBullets gstate)
    
    hasInstaKill = any (\p -> case apType p of InstaKill _ -> True; _ -> False) (pPowerups $ gsPlayer gstate)
    
    (hitBullets, survivingBullets', enemies', pickups', gen', score', kills') =
      processCollisions playerBullets (gsEnemies gstate) [] [] (gsRandGen gstate) 0 0 hasInstaKill
    
    survivingBullets = survivingBullets' ++ otherBullets
    (survivingEnemies, newEnemies) = partition (\e -> eHealth e > 0) enemies'
    newPickups = pickups'
    scoreGained = score'
    enemiesKilled = kills'
    finalGen = gen'

-- | Process bullet-enemy collisions recursively
processCollisions :: [Bullet] -> [Enemy] -> [Bullet] -> [Pickup] -> StdGen -> Int -> Int -> Bool
                  -> ([Bullet], [Bullet], [Enemy], [Pickup], StdGen, Int, Int)
processCollisions [] enemies survivingBullets pickups gen score kills _ =
  ([], survivingBullets, enemies, pickups, gen, score, kills)
processCollisions (b:bs) enemies survivingBullets pickups gen score kills instaKill =
  case findCollision b enemies of
    Nothing -> processCollisions bs enemies (b:survivingBullets) pickups gen score kills instaKill
    Just (hitEnemy, otherEnemies) ->
      let damage = if instaKill then 1000 else bDamage b
          newHealth = eHealth hitEnemy - damage
          damagedEnemy = hitEnemy { eHealth = newHealth }
          -- Always spawn pickup on hit for Martians, only on death for others
          (pickup, gen') = if eType hitEnemy == Martian
                          then maybeSpawnPickup (ePos hitEnemy) gen
                          else if newHealth <= 0
                               then maybeSpawnPickup (ePos hitEnemy) gen
                               else (Nothing, gen)
          newPickups = maybe pickups (:pickups) pickup
          (newScore, newKills) = if newHealth <= 0
                                 then (score + enemyScore (eType hitEnemy), kills + 1)
                                 else (score, kills)
          updatedEnemies = if newHealth <= 0 
                          then otherEnemies  -- Remove dead enemies
                          else damagedEnemy : otherEnemies
      in processCollisions bs updatedEnemies survivingBullets newPickups gen' newScore newKills instaKill

-- | Find collision between bullet and enemies
findCollision :: Bullet -> [Enemy] -> Maybe (Enemy, [Enemy])
findCollision _ [] = Nothing
findCollision bullet (e:es)
  | distance (bPos bullet) (ePos e) < 25 = Just (e, es)
  | otherwise = case findCollision bullet es of
      Nothing -> Nothing
      Just (hit, rest) -> Just (hit, e:rest)

-- | Bullet-Asteroid collisions
handleBulletAsteroidCollisions :: GameState -> GameState
handleBulletAsteroidCollisions gstate = gstate
  { gsBullets = survivingBullets
  , gsAsteroids = survivingAsteroids ++ newAsteroids
  , gsScore = gsScore gstate + scoreGained
  , gsNextId = newId
  , gsWave = wave { killsThisWave = killsThisWave wave + asteroidsDestroyed }
  }
  where
    wave = gsWave gstate
    playerBullets = filter (\b -> bOwner b == FromPlayer) (gsBullets gstate)
    otherBullets = filter (\b -> bOwner b /= FromPlayer) (gsBullets gstate)
    
    hasInstaKill = any (\p -> case apType p of InstaKill _ -> True; _ -> False) (pPowerups $ gsPlayer gstate)
    
    (hitBullets, survivingBullets', asteroids', newAsts, score', destroyed, newId) =
      processAsteroidCollisions playerBullets (gsAsteroids gstate) [] [] 0 0 (gsNextId gstate) hasInstaKill
    
    survivingBullets = survivingBullets' ++ otherBullets
    survivingAsteroids = filter (\a -> case aSize a of Small -> False; _ -> True) asteroids'
    newAsteroids = newAsts
    scoreGained = score'
    asteroidsDestroyed = destroyed

-- | Process bullet-asteroid collisions
processAsteroidCollisions :: [Bullet] -> [Asteroid] -> [Bullet] -> [Asteroid] -> Int -> Int -> EntityId -> Bool
                          -> ([Bullet], [Bullet], [Asteroid], [Asteroid], Int, Int, EntityId)
processAsteroidCollisions [] asteroids survivingBullets newAsts score destroyed nextId _ =
  ([], survivingBullets, asteroids, newAsts, score, destroyed, nextId)
processAsteroidCollisions (b:bs) asteroids survivingBullets newAsts score destroyed nextId instaKill =
  case findAsteroidCollision b asteroids of
    Nothing -> processAsteroidCollisions bs asteroids (b:survivingBullets) newAsts score destroyed nextId instaKill
    Just (hitAst, otherAsts) ->
      let (fragments, score', destroyed', nextId') = splitAsteroid hitAst nextId
          newScore = score + score'
          newDestroyed = destroyed + destroyed'
          updatedAsts = if instaKill then otherAsts else otherAsts
          updatedNewAsts = if instaKill then newAsts else newAsts ++ fragments
      in processAsteroidCollisions bs updatedAsts survivingBullets updatedNewAsts newScore newDestroyed nextId' instaKill

-- | Find collision between bullet and asteroids
findAsteroidCollision :: Bullet -> [Asteroid] -> Maybe (Asteroid, [Asteroid])
findAsteroidCollision _ [] = Nothing
findAsteroidCollision bullet (a:as)
  | distance (bPos bullet) (aPos a) < asteroidRadius (aSize a) = Just (a, as)
  | otherwise = case findAsteroidCollision bullet as of
      Nothing -> Nothing
      Just (hit, rest) -> Just (hit, a:rest)

-- | Split asteroid into smaller pieces
splitAsteroid :: Asteroid -> EntityId -> ([Asteroid], Int, Int, EntityId)
splitAsteroid asteroid nextId = case aSize asteroid of
  Large -> (fragments Medium 2, 10, 0, nextId + 2)
  Medium -> (fragments Small 2, 20, 0, nextId + 2)
  Small -> ([], 30, 1, nextId)
  where
    fragments size count = take count
      [ Asteroid
          { aId = nextId + fromIntegral i
          , aPos = aPos asteroid
          , aVel = rotateVec (fromIntegral i * (360 / fromIntegral count)) (scaleVec 1.2 (aVel asteroid))
          , aSize = size
          }
      | i <- [0..count-1]
      ]

-- | Player-Pickup collisions
handlePlayerPickupCollisions :: GameState -> GameState
handlePlayerPickupCollisions gstate = gstate
  { gsPickups = survivingPickups
  , gsPlayer = newPlayer
  , gsScore = newScore
  , gsEnemies = newEnemies
  , gsAsteroids = newAsteroids
  }
  where
    player = gsPlayer gstate
    pickups = gsPickups gstate
    
    (collectedPickups, survivingPickups) = partition (collidesWithPlayer player) pickups
    
    (newPlayer, newScore, newEnemies, newAsteroids) =
      foldl applyPickup (player, gsScore gstate, gsEnemies gstate, gsAsteroids gstate) collectedPickups
    
    collidesWithPlayer p pickup = distance (pPos p) (puPos pickup) < 30

-- | Apply pickup effect
applyPickup :: (Player, Int, [Enemy], [Asteroid]) -> Pickup -> (Player, Int, [Enemy], [Asteroid])
applyPickup (player, score, enemies, asteroids) pickup = case puType pickup of
  HealthDrop amt -> (player { pHealth = min (pMaxHealth player) (pHealth player + amt) }, score, enemies, asteroids)
  ScoreStar amt -> (player, score + amt, enemies, asteroids)
  InstaKill dur -> (player { pPowerups = ActivePowerup (InstaKill dur) dur : pPowerups player }, score, enemies, asteroids)
  Nuke -> (player, score + 100, [], [])

-- | Enemy-Player collisions
handleEnemyPlayerCollisions :: GameState -> GameState
handleEnemyPlayerCollisions gstate = gstate
  { gsPlayer = newPlayer
  , gsEnemies = gsEnemies gstate
  }
  where
    player = gsPlayer gstate
    collidingEnemies = filter (collidesWithPlayer player) (gsEnemies gstate)
    damage = length collidingEnemies * 10
    newPlayer = player { pHealth = pHealth player - damage }
    
    collidesWithPlayer p enemy = distance (pPos p) (ePos enemy) < 30

-- | Asteroid-Player collisions
handleAsteroidPlayerCollisions :: GameState -> GameState
handleAsteroidPlayerCollisions gstate = gstate { gsPlayer = newPlayer }
  where
    player = gsPlayer gstate
    collidingAsteroids = filter (collidesWithPlayer player) (gsAsteroids gstate)
    damage = sum [asteroidDamage (aSize a) | a <- collidingAsteroids]
    newPlayer = player { pHealth = pHealth player - damage }
    
    collidesWithPlayer p asteroid = distance (pPos p) (aPos asteroid) < asteroidRadius (aSize asteroid)
    asteroidDamage Large = 30
    asteroidDamage Medium = 20
    asteroidDamage Small = 10

-- | Wave spawning logic
updateWaveSpawning :: Float -> GameState -> GameState
updateWaveSpawning dt gstate = gstate
  { gsWave = newWave
  , gsEnemies = gsEnemies gstate ++ newEnemies
  , gsAsteroids = gsAsteroids gstate ++ newAsteroids
  , gsRandGen = gen'''
  , gsNextId = newId
  }
  where
    wave = gsWave gstate
    newTimer = spawnTimer wave - dt
    
    shouldSpawn = newTimer <= 0 && length (gsEnemies gstate) < maxEnemies wave
    
    (newEnemies, newAsteroids, gen''', newWave, newId) = if shouldSpawn
      then let (enemy, gen') = spawnRandomEnemy (gsNextId gstate) (gsRandGen gstate) (waveNumber wave)
               (asteroid, gen'') = spawnRandomAsteroid (gsNextId gstate + 1) gen' (waveNumber wave)
               resetWave = wave { spawnTimer = spawnCooldown wave }
           in ([enemy], [asteroid], gen'', resetWave, gsNextId gstate + 2)
      else ([], [], gsRandGen gstate, wave { spawnTimer = newTimer }, gsNextId gstate)

-- | Check if wave is complete
checkWaveCompletion :: GameState -> GameState
checkWaveCompletion gstate
  | killsThisWave wave >= killsRequired wave = gstate  -- Remove the null check to progress waves
      { gsWave = nextWave
      }
  | otherwise = gstate
  where
    wave = gsWave gstate
    wNum = waveNumber wave + 1
    nextWave = WaveState
      { waveNumber = wNum
      , killsThisWave = 0
      , killsRequired = 5 + wNum * 2
      , spawnCooldown = max 0.5 (2.0 - fromIntegral wNum * 0.1)
      , spawnTimer = 0.0  -- Start spawning immediately in new wave
      , maxEnemies = min 10 (3 + wNum)
      }

-- | Update time and powerup durations
updateTimeAndPowerups :: Float -> GameState -> GameState
updateTimeAndPowerups dt gstate = gstate
  { gsTimeElapsed = gsTimeElapsed gstate + dt
  , gsPlayer = player { pPowerups = activePowerups }
  }
  where
    player = gsPlayer gstate
    updated = map (\p -> p { apRemaining = apRemaining p - dt }) (pPowerups player)
    activePowerups = filter (\p -> apRemaining p > 0) updated

-- | Check if game is over
checkGameOver :: GameState -> GameState
checkGameOver gstate
  | pHealth (gsPlayer gstate) <= 0 = gstate 
      { gsGameOver = True
      , gsGameMode = GameOverScreen
      }
  | otherwise = gstate

-- | Spawn random enemy
spawnRandomEnemy :: EntityId -> StdGen -> Int -> (Enemy, StdGen)
spawnRandomEnemy eid gen wave =
  let (x, gen') = randomR (-screenWidth/2 + 50, screenWidth/2 - 50) gen
      -- Higher waves increase chance of stronger enemies
      (typeRoll, gen'') = randomR (0, 100) gen' :: (Int, StdGen)
      -- Enemy type selection based on wave and random roll
      enemyType = selectEnemyType wave typeRoll
      -- Each type has unique health and movement
      (vx, gen''') = randomR (-50, 50) gen''  -- For horizontal movement
      baseSpeed = enemySpeed + fromIntegral wave * 5
      (health, vel) = case enemyType of
        Alien -> (1, (0, -baseSpeed * 1.2))  -- Fast but weak
        Martian -> (2, (0, -baseSpeed))      -- Standard
        UFO -> (3, (vx, -baseSpeed * 0.8))   -- Tough, moves horizontally
      enemy = Enemy
        { eId = eid
        , ePos = (x, screenHeight/2 + 30)
        , eVel = vel
        , eHealth = health
        , eType = enemyType
        }
  in (enemy, gen''')
  where
    selectEnemyType :: Int -> Int -> EnemyType
    selectEnemyType waveNum roll
      | waveNum <= 2 = Alien                            -- Waves 1-2: Only Aliens
      | waveNum <= 4 = if roll < 70 then Alien else Martian  -- Waves 3-4: Mostly Aliens, some Martians
      | waveNum <= 6 = case roll of                     -- Waves 5-6: Mix of all three
          r | r < 50 -> Alien
            | r < 80 -> Martian
            | otherwise -> UFO
      | otherwise = case roll of                        -- Wave 7+: More tough enemies
          r | r < 30 -> Alien
            | r < 70 -> Martian
            | otherwise -> UFO

-- | Spawn random asteroid
spawnRandomAsteroid :: EntityId -> StdGen -> Int -> (Asteroid, StdGen)
spawnRandomAsteroid eid gen wave =
  let (x, gen') = randomR (-screenWidth/2, screenWidth/2) gen
      (vx, gen'') = randomR (-50, 50) gen'
      (vy, gen''') = randomR (-100, -50) gen''
      asteroid = Asteroid
        { aId = eid
        , aPos = (x, screenHeight/2 + 50)
        , aVel = (vx, vy - fromIntegral wave * 3)
        , aSize = Large
        }
  in (asteroid, gen''')

-- | Maybe spawn a pickup
maybeSpawnPickup :: Vec -> StdGen -> (Maybe Pickup, StdGen)
maybeSpawnPickup pos gen =
  let (chance, gen') = randomR (0, 1) gen :: (Float, StdGen)
  in if chance < 0.2
     then let (pickupType, gen'') = randomPickupType gen'
              pickup = Pickup
                { puId = 9999
                , puPos = pos
                , puVel = (0, -50)
                , puType = pickupType
                , puTTL = Just 10.0
                }
          in (Just pickup, gen'')
     else (Nothing, gen')

randomPickupType :: StdGen -> (PickupType, StdGen)
randomPickupType gen =
  let (idx, gen') = randomR (0, 3) gen :: (Int, StdGen)
  in case idx of
    0 -> (HealthDrop 25, gen')
    1 -> (InstaKill 5.0, gen')
    2 -> (Nuke, gen')
    _ -> (ScoreStar 50, gen')

-- | Helper functions
distance :: Vec -> Vec -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

addVec :: Vec -> Vec -> Vec
addVec (x1, y1) (x2, y2) = (x1+x2, y1+y2)

scaleVec :: Float -> Vec -> Vec
scaleVec s (x, y) = (s*x, s*y)

rotateVec :: Float -> Vec -> Vec
rotateVec deg (x, y) =
  let rad = deg * pi / 180
      cos' = cos rad
      sin' = sin rad
  in (x * cos' - y * sin', x * sin' + y * cos')

clamp :: Float -> Float -> Float -> Float
clamp minVal maxVal = max minVal . min maxVal

wrapHorizontal :: Vec -> Vec
wrapHorizontal (x, y)
  | x < -screenWidth/2 = (screenWidth/2, y)
  | x > screenWidth/2 = (-screenWidth/2, y)
  | otherwise = (x, y)

asteroidRadius :: AsteroidSize -> Float
asteroidRadius Large = 40
asteroidRadius Medium = 25
asteroidRadius Small = 15

enemyScore :: EnemyType -> Int
enemyScore Alien = 10
enemyScore Martian = 25
enemyScore UFO = 50

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event gstate = case gsGameMode gstate of
  Menu -> return $ handleMenuInput event gstate
  -- In EnteringName we need to allow saving which performs IO (writeFile),
  -- so route events to an IO handler that can call `saveHighScore`.
  EnteringName -> handleNameInputIO event gstate
  GameOverScreen -> handleGameOverInput event gstate
  Playing -> return $ handlePlayingInput event gstate
  _ -> return gstate

-- | Handle menu input
handleMenuInput :: Event -> GameState -> GameState
handleMenuInput (EventKey (SpecialKey KeySpace) Down _ _) gstate =
  startNewGame gstate
handleMenuInput _ gstate = gstate

-- | Start a new game
startNewGame :: GameState -> GameState
startNewGame gstate = initialState
  { gsGameMode = Playing  -- Set to Playing mode
  , gsHighScores = gsHighScores gstate  -- Keep high scores
  , gsGameOver = False  -- Ensure game over is reset
  , gsPaused = False  -- Ensure game isn't paused
  , gsRandGen = gsRandGen gstate  -- Keep random generator state
  }

-- | Handle name entry input (IO-capable)
handleNameInputIO :: Event -> GameState -> IO GameState
handleNameInputIO (EventKey key keyState _ _) gstate = 
    case (key, keyState) of
        -- Handle Enter key
        (SpecialKey KeyEnter, Down) ->
            if not (null (gsPlayerName gstate))
                then saveHighScore gstate
                else return $ gstate { gsGameMode = Menu }
        
        -- Handle Backspace key (both press and release)
        (SpecialKey KeyBackspace, _) ->
            return $ gstate { gsPlayerName = if null (gsPlayerName gstate) 
                                            then "" 
                                            else init (gsPlayerName gstate) }
        
        -- Handle Delete key (both press and release)
        (SpecialKey KeyDelete, _) ->
            return $ gstate { gsPlayerName = if null (gsPlayerName gstate) 
                                            then "" 
                                            else init (gsPlayerName gstate) }
        
        -- Handle character input (only on key press)
        (Char c, Down) ->
            if length (gsPlayerName gstate) < 15
                then return $ gstate { gsPlayerName = gsPlayerName gstate ++ [c] }
                else return gstate
        
        -- Ignore all other keys
        _ -> return gstate
handleNameInputIO _ gstate = return gstate

-- | Handle game over input
handleGameOverInput :: Event -> GameState -> IO GameState
handleGameOverInput (EventKey key Down _ _) gstate = case key of
  Char 'r' -> do
    let newState = startNewGame gstate
    return $ newState { gsGameMode = Playing }  -- Explicitly set mode to Playing
  Char 'm' -> return $ gstate { gsGameMode = Menu }
  Char 's' -> if qualifiesForHighScore (gsScore gstate) (gsHighScores gstate)
              then return $ gstate { gsGameMode = EnteringName, gsPlayerName = "" }
              else return gstate
  SpecialKey KeyEnter -> if not (null (gsPlayerName gstate)) && gsGameMode gstate == EnteringName
                        then saveHighScore gstate
                        else return gstate
  _ -> return gstate
handleGameOverInput _ gstate = return gstate

-- | Handle playing input
handlePlayingInput :: Event -> GameState -> GameState
handlePlayingInput (EventKey key keyState _ _) gstate = case (key, keyState) of
    -- Pause
    (Char 'p', Down) -> gstate { gsPaused = not (gsPaused gstate) }
    
    -- Move left (arrow key or 'A')
    (SpecialKey KeyLeft, Down) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (-playerSpeed, 0) } }
    (SpecialKey KeyLeft, Up) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (0, 0) } }
    (Char 'a', Down) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (-playerSpeed, 0) } }
    (Char 'a', Up) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (0, 0) } }
    
    -- Move right (arrow key or 'D')
    (SpecialKey KeyRight, Down) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (playerSpeed, 0) } }
    (SpecialKey KeyRight, Up) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (0, 0) } }
    (Char 'd', Down) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (playerSpeed, 0) } }
    (Char 'd', Up) -> gstate { gsPlayer = (gsPlayer gstate) { pVel = (0, 0) } }
    
    -- Shoot
    (SpecialKey KeySpace, Down) -> shootBullet gstate
    
    -- Any other key
    _ -> gstate
handlePlayingInput _ gstate = gstate  -- Handle all non-EventKey events

-- | Check if score qualifies for top 10
qualifiesForHighScore :: Int -> [HighScore] -> Bool
qualifiesForHighScore score highScores
  | length highScores < 10 = True
  | otherwise = score > minimum (map hsScore highScores)

-- | Save high score to file
saveHighScore :: GameState -> IO GameState
saveHighScore gstate = do
  let newScore = HighScore (gsPlayerName gstate) (gsScore gstate)
  let updatedScores = take 10 $ reverse $ sort' $ newScore : gsHighScores gstate
  writeFile "src/highscores.txt" (show updatedScores)
  return $ gstate 
    { gsHighScores = updatedScores
    , gsGameMode = Menu
    , gsPlayerName = ""
    }
  where
    sort' = sortBy (\a b -> compare (hsScore a) (hsScore b))

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) = sortBy cmp smaller ++ [x] ++ sortBy cmp larger
  where
    smaller = filter (\y -> cmp y x == LT) xs
    larger = filter (\y -> cmp y x /= LT) xs

-- | Shoot a bullet
shootBullet :: GameState -> GameState
shootBullet gstate
  | gsGameOver gstate = gstate
  | gsPaused gstate = gstate
  | pFireCooldown player > 0 = gstate
  | otherwise = gstate
      { gsBullets = newBullet : gsBullets gstate
      , gsPlayer = player { pFireCooldown = playerFireRate }
      , gsNextId = gsNextId gstate + 1
      }
  where
    player = gsPlayer gstate
    newBullet = Bullet
      { bId = gsNextId gstate
      , bPos = pPos player
      , bVel = (0, bulletSpeed)
      , bOwner = FromPlayer
      , bDamage = 1
      }