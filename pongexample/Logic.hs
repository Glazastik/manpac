module Logic where
import qualified Data.Set as S
import Haste.Graphics.Canvas (Point, Vector, Rect (..))

-- | The state of our game.
data GameState = GameState {
    ballPos     :: Point,
    ballDir     :: Vector,
    leftPaddle  :: Point,
    rightPaddle :: Point,
    leftScore   :: Int,
    rightScore  :: Int
  }

data Player = West | East
  deriving (Eq, Show)

-- | Width and height of the playing field.
width, height :: Double
width = 800
height = 500

-- | How big is the ball?
ballRadius :: Double
ballRadius = 5

-- | Height of the paddle.
paddleWidth, paddleHeight :: Double
paddleWidth = 10
paddleHeight = 50

-- | How far will the paddle move in one tick?
paddleSpeed :: Double
paddleSpeed = 10

-- | Move a point by a certain velocity.
move :: Point -> Vector -> Point
move (x, y) (xv, yv) = (x + xv, y + yv)

-- | Ensure that the point is inside the rectangle.
clamp :: Point -> Rect -> Point
clamp (x, y) (Rect xMin yMin xMax yMax) =
  (min (max x xMin) xMax, min (max y yMin) yMax)

-- | Component-wise multiplication of vectors.
scale :: Vector -> Vector -> Vector
scale (x, y) (sx, sy) = (x*sx, y*sy)

-- | Is the point inside the rectangle?
inside :: Point -> Rect -> Bool
inside (x, y) (Rect x1 y1 x2 y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

-- | Create a rectangle for a paddle.
paddleRect :: Point -> Rect
paddleRect (x, y) = Rect x y (x+paddleWidth) (y+paddleHeight)

-- | Bounce the ball against the walls.
bounceWalls :: GameState -> GameState
bounceWalls state
  | y > height || y < 0 = state {ballDir = ballDir state `scale` (1, -1)}
  | otherwise           = state
  where
    (_, y) = ballPos state

-- | Bounce the ball against the paddles.
bouncePaddles :: GameState -> GameState
bouncePaddles state
  | ballPos state `inside` paddleRect (leftPaddle state) ||
    ballPos state `inside` paddleRect (rightPaddle state) =
      state {ballDir = ballDir state `scale` (-1, 1)}
  | otherwise =
      state

-- | Update the ball's position with its velocity.
moveBall :: GameState -> GameState
moveBall state = state {ballPos = ballPos state `move` ballDir state}

-- | Update the paddles depending on the currently pressed keys.
movePaddles :: S.Set Char -> GameState -> GameState
movePaddles keys state = state {
      leftPaddle  = movePaddle 'W' 'S' (leftPaddle state) `clamp` playingField,
      rightPaddle = movePaddle 'O' 'L' (rightPaddle state) `clamp` playingField
    }
  where
    playingField = Rect 0 0 width height
    movePaddle up down paddle
      | up `S.member` keys   = paddle `move` (0, -paddleSpeed)
      | down `S.member` keys = paddle `move` (0, paddleSpeed)
      | otherwise            = paddle

-- | Check if there this round has a winner yet.
checkWinner :: GameState -> Maybe Player
checkWinner state
  | fst (ballPos state) < 0     = Just East
  | fst (ballPos state) > width = Just West
  | otherwise                   = Nothing

-- | The ball's initial state.
initialState :: GameState
initialState = GameState {
    ballPos     = (width/2, height/2),
    ballDir     = (0, 0),
    leftPaddle  = (50, height/2),
    rightPaddle = (width-50, height/2),
    leftScore   = 0,
    rightScore  = 0
  }