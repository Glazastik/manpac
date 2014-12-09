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
manPacRadius = 50

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

-- | Update the paddles depending on the currently pressed keys.
moveManPac :: S.Set Char -> GameState -> GameState
moveManPac keys state = state {
      manPacPos  = movePac 'W' 'S' 'A' 'D' (manPacPos state) `clamp` playingField
    }
  where
    playingField = Rect 0 0 width height
    movePac up down left right pac
      | up `S.member` keys   = pac `move` (0, -manPacSpeed)
      | down `S.member` keys = pac `move` (0, manPacSpeed)
      | left `S.member` keys = pac `move` (-manPacSpeed, 0)
      | right `S.member`keys = pac `move` (manPacSpeed, 0)
      | otherwise            = pac


initialState :: GameState
initialState = GameState {
	manPacPos = (width/2, height/2),
    manPacDir = (0,0)
}
