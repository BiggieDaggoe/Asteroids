{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Graphics.Gloss (Picture)
import GHC.Generics (Generic)
import System.Random (StdGen, mkStdGen)

type Vec = (Float, Float)
type Seconds = Float

newtype EntityId = EntityId Int
  deriving (Eq, Ord, Show, Num)

-- | Main game state
data GameState = GameState
  { gsPlayer        :: Player
  , gsEnemies       :: [Enemy]
  , gsAsteroids     :: [Asteroid]
  , gsBullets       :: [Bullet]
  , gsPickups       :: [Pickup]
  , gsWave          :: WaveState
  , gsScore         :: Int
  , gsTimeElapsed   :: Seconds
  , gsRandGen       :: StdGen
  , gsPaused        :: Bool
  , gsNextId        :: EntityId
  , gsGameOver      :: Bool
  , gsGameMode      :: GameMode
  , gsHighScores    :: [HighScore]
  , gsPlayerName    :: String
  } deriving (Show, Generic)

-- | Game mode state
data GameMode = Menu | Playing | GameOverScreen | EnteringName
  deriving (Show, Eq, Generic)

-- | High score entry
data HighScore = HighScore
  { hsName  :: String
  , hsScore :: Int
  } deriving (Show, Read, Generic)

-- | Player state
data Player = Player
  { pId           :: EntityId
  , pPos          :: Vec
  , pVel          :: Vec
  , pHealth       :: Int
  , pMaxHealth    :: Int
  , pFireCooldown :: Seconds
  , pPowerups     :: [ActivePowerup]
  } deriving (Show, Generic)

-- | Enemy types and state
data EnemyType = Alien | Martian | UFO 
  deriving (Eq, Show, Enum, Bounded)

data Enemy = Enemy
  { eId      :: EntityId
  , ePos     :: Vec
  , eVel     :: Vec
  , eHealth  :: Int
  , eType    :: EnemyType
  } deriving (Show, Generic)

-- | Asteroid state
data Asteroid = Asteroid
  { aId     :: EntityId
  , aPos    :: Vec
  , aVel    :: Vec
  , aSize   :: AsteroidSize
  } deriving (Show, Generic)

data AsteroidSize = Large | Medium | Small 
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Bullet state
data Bullet = Bullet
  { bId     :: EntityId
  , bPos    :: Vec
  , bVel    :: Vec
  , bOwner  :: BulletOwner
  , bDamage :: Int
  } deriving (Show, Generic)

data BulletOwner = FromPlayer | FromEnemy EntityId 
  deriving (Show, Eq, Generic)

-- | Pickup types and state
data PickupType = HealthDrop Int
                | InstaKill Seconds
                | Nuke
                | ScoreStar Int
                deriving (Show, Generic)

data Pickup = Pickup
  { puId    :: EntityId
  , puPos   :: Vec
  , puVel   :: Vec
  , puType  :: PickupType
  , puTTL   :: Maybe Seconds
  } deriving (Show, Generic)

-- | Active powerups on player
data ActivePowerup = ActivePowerup
  { apType      :: PickupType
  , apRemaining :: Seconds
  } deriving (Show, Generic)

-- | Wave progression state
data WaveState = WaveState
  { waveNumber     :: Int
  , killsThisWave  :: Int
  , killsRequired  :: Int
  , spawnCooldown  :: Seconds
  , spawnTimer     :: Seconds
  , maxEnemies     :: Int
  } deriving (Show, Generic)

-- | Game constants
screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

playerSpeed :: Float
playerSpeed = 300.0

playerFireRate :: Seconds
playerFireRate = 0.2

bulletSpeed :: Float
bulletSpeed = 500.0

enemySpeed :: Float
enemySpeed = 80.0

asteroidSpeed :: Float
asteroidSpeed = 50.0

-- | Initial game state
initialState :: GameState
initialState = GameState
  { gsPlayer = Player
      { pId = 0
      , pPos = (0, -screenHeight / 2 + 50)
      , pVel = (0, 0)
      , pHealth = 100
      , pMaxHealth = 100
      , pFireCooldown = 0
      , pPowerups = []
      }
  , gsEnemies = []
  , gsAsteroids = []
  , gsBullets = []
  , gsPickups = []
  , gsWave = WaveState
      { waveNumber = 1
      , killsThisWave = 0
      , killsRequired = 5
      , spawnCooldown = 2.0
      , spawnTimer = 1.0
      , maxEnemies = 3
      }
  , gsScore = 0
  , gsTimeElapsed = 0
  , gsRandGen = mkStdGen 42
  , gsPaused = False
  , gsNextId = 1
  , gsGameOver = False
  , gsGameMode = Menu
  , gsHighScores = []
  , gsPlayerName = ""
  }

-- | Input state tracking
data InputState = InputState
  { isMoveLeft  :: Bool
  , isMoveRight :: Bool
  , isShooting  :: Bool
  } deriving (Show)

initialInputState :: InputState
initialInputState = InputState False False False