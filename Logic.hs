module Logic where
import Haste.Graphics.Canvas

-- | The state of our game.
data GameState = GameState {
    manPacPos :: Point
  }

-- Width and height of the playing field.
width, height :: Double
width = 800
height = 500