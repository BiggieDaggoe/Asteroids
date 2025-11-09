module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (partition)

-- | Main game step function
step :: Float -> GameState -> IO GameState
step secs gstate
  | gsGameOver gstate = return gstate
  | gsPaused gstate = return gstate
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
updateEnemies dt gstate = gstate { gsEnemies = newEnemies }
  where
    enemies = gsEnemies gstate
    newEnemies = map (moveEnemy dt) enemies
    
    moveEnemy dt' enemy = enemy
      { ePos = addVec (ePos enemy) (scaleVec dt' (eVel enemy))
      }

-- | Update all asteroids
updateAsteroids :: Float -> GameState -> GameState
updateAsteroids dt gstate = gstate { gsAsteroids = newAsteroids }
  where
    asteroids = gsAsteroids gstate
    newAsteroids = map (moveAsteroid dt) asteroids
    
    moveAsteroid dt' asteroid = asteroid
      { aPos = wrapPosition (addVec (aPos asteroid) (scaleVec dt' (aVel asteroid)))
      }

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
          (pickup, gen') = maybeSpawnPickup (ePos hitEnemy) gen
          newPickups = maybe pickups (:pickups) pickup
          (newScore, newKills) = if newHealth <= 0
                                 then (score + enemyScore (eType hitEnemy), kills + 1)
                                 else (score, kills)
          updatedEnemies = damagedEnemy : otherEnemies
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
  | killsThisWave wave >= killsRequired wave && null (gsEnemies gstate) = gstate
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
      , spawnTimer = 1.0
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
  | pHealth (gsPlayer gstate) <= 0 = gstate { gsGameOver = True }
  | otherwise = gstate

-- | Spawn random enemy
spawnRandomEnemy :: EntityId -> StdGen -> Int -> (Enemy, StdGen)
spawnRandomEnemy eid gen wave =
  let (x, gen') = randomR (-screenWidth/2 + 50, screenWidth/2 - 50) gen
      (typeIdx, gen'') = randomR (0, min 2 wave `div` 3) gen'
      enemyType = toEnum typeIdx :: EnemyType
      health = case enemyType of
        Alien -> 1
        Martian -> 2
        UFO -> 3
      enemy = Enemy
        { eId = eid
        , ePos = (x, screenHeight/2 + 30)
        , eVel = (0, -enemySpeed - fromIntegral wave * 5)
        , eHealth = health
        , eType = enemyType
        }
  in (enemy, gen'')

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

wrapPosition :: Vec -> Vec
wrapPosition (x, y)
  | x < -screenWidth/2 = (screenWidth/2, y)
  | x > screenWidth/2 = (-screenWidth/2, y)
  | y < -screenHeight/2 = (x, screenHeight/2)
  | y > screenHeight/2 = (x, -screenHeight/2)
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
input event gstate = return $ inputKey event gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate =
  gstate { gsPaused = not (gsPaused gstate) }
inputKey (EventKey (Char 'r') Down _ _) gstate
  | gsGameOver gstate = initialState
  | otherwise = gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate =
  gstate { gsPlayer = (gsPlayer gstate) { pVel = (-playerSpeed, 0) } }
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate =
  gstate { gsPlayer = (gsPlayer gstate) { pVel = (0, 0) } }
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =
  gstate { gsPlayer = (gsPlayer gstate) { pVel = (playerSpeed, 0) } }
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate =
  gstate { gsPlayer = (gsPlayer gstate) { pVel = (0, 0) } }
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate =
  shootBullet gstate
inputKey _ gstate = gstate

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