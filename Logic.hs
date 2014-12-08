module Logic where
import Haste.Graphics.Canvas

-- | The state of our game.
data GameState = GameState {
    manPacPos :: Point
    manPacDir :: Vector
  }

-- Width and height of the playing field.
width, height :: Double
width  = 800
height = 500

-- How big manPac is
manPacRadius :: Double
manPacRadius = 50

-- | Move a point by a certain velocity.
move :: Point -> Vector -> Point
move (x, y) (xv, yv) = (x + xv, y + yv)

-- | Ensure that the point is inside the rectangle.
clamp :: Point -> Rect -> Point
clamp (x, y) (Rect xMin yMin xMax yMax) =
  (min (max x xMin) xMax, min (max y yMin) yMax)

initialState :: GameState
initialState = GameState {
	manPacPos = (width/2, height/2),
    manPacDir = (0,0)
}