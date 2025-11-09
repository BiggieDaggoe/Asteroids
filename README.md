# Space Shooter Game

A classic arcade-style space shooter game built with Haskell and Gloss, featuring multiple enemy types, power-ups, and wave-based progression.

## Installation and Running

### Prerequisites
- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell package manager)

### Running the Game
1. Clone this repository
2. Navigate to the project directory
3. Run the following commands:
```bash
cabal update
cabal build
cabal run
```

## How to Play

### Controls
- **A** or **Left Arrow**: Move left
- **D** or **Right Arrow**: Move right
- **Space**: Shoot
- **P**: Pause game
- **R**: Restart (when game over)
- **M**: Return to menu (when game over)
- **S**: Save high score (when game over)

### Gameplay Features

#### Enemy Types
- **Aliens**: Fast but weak enemies that move straight down
- **Martians**: Medium-strength enemies that respawn at the top when they reach the bottom
- **UFOs**: Tough enemies that move horizontally and shoot at the player

#### Power-ups
Various power-ups can be collected during gameplay:
- **Health Drop**: Restores 25 HP
- **Score Star**: Adds 50 points
- **Insta-Kill**: Temporarily allows one-shot kills on all enemies
- **Nuke**: Clears all enemies from the screen

#### Wave System
- Game progresses through increasingly difficult waves
- Each wave requires defeating a certain number of enemies
- Enemy types become more varied and challenging in later waves
- Enemy speed and spawn rates increase with wave progression

#### Scoring System
- Alien: 10 points
- Martian: 25 points
- UFO: 50 points
- Breaking asteroids also awards points
- High scores are saved locally

### Game Mechanics

#### Player
- Start with 100 HP
- Can move horizontally at the bottom of the screen
- Shoots bullets upward
- Must dodge enemy bullets, enemies, and asteroids

#### Asteroids
- Spawn as Large asteroids
- When shot, a Large asteroid splits into two smaller asteroids
- Each small asteroid is destroyed when shot
- Deal damage based on size (Large: 30, Small: 10)
- Breaking asteroids awards points (Large: 10, Small: 30 points)

#### Enemy Behavior
- **Aliens**: Move straight down quickly
- **Martians**: Move down and respawn at top
- **UFOs**: Move horizontally and shoot aimed bullets at player

### Tips
- Dodge UFO bullets by constantly moving
- Collect power-ups whenever safe
- Use Insta-Kill power-ups strategically during tough waves
- Break asteroids from a safe distance

## High Scores
- Top 10 scores are saved
- Enter your name after achieving a high score
- Scores persist between game sessions