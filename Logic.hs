module Logic where
import qualified Data.Set as S
import Haste.Graphics.Canvas

-- | The state of our game.
data GameState = GameState {
    manPacPos :: Point,
    manPacDir :: Vector
  }

-- Width and height of the playing field.
width, height :: Double
width  = 800
height = 500

-- How big manPac is
manPacRadius :: Double
manPacRadius = 15

-- How fast is manPac
manPacSpeed :: Double
manPacSpeed = 10

-- | Move a point by a certain velocity.
move :: Point -> Vector -> Point
move (x, y) (xv, yv) = (x + xv, y + yv)

-- | Ensure that the point is inside the rectangle.
clamp :: Point -> Rect -> Point
clamp (x, y) (Rect xMin yMin xMax yMax) =
  (min (max x xMin) xMax, min (max y yMin) yMax)

-- | Update the ball's position with its velocity.
moveManPac :: GameState -> GameState
moveManPac state = state {manPacPos = manPacPos state `move` manPacDir state}

-- | Update the paddles depending on the currently pressed keys.
changeManPacDir :: S.Set Char -> GameState -> GameState
changeManPacDir keys state = pacDir 'W' 'S' 'A' 'D' 
  where
    pacDir up down left right
      | up `S.member` keys   = state { manPacDir = (0, -manPacSpeed) }
      | down `S.member` keys = state { manPacDir = (0, manPacSpeed) }
      | left `S.member` keys = state { manPacDir = (-manPacSpeed, 0) }
      | right `S.member`keys = state { manPacDir = (manPacSpeed, 0) }
      | otherwise            = state


initialState :: GameState
initialState = GameState {
	manPacPos = (width/2, height/2),
    manPacDir = (0,0)
}

