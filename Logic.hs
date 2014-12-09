module Logic where
import qualified Data.Set as S
import Haste.Graphics.Canvas

-- | The state of our game.
data GameState = GameState {
    manPacPos :: Point,
    manPacDir :: Vector,
    wallBlocks :: [Rect]
  }

type Wall = Rect

-- Width and height of the playing field.
width, height :: Double
width  = 32 * manPacRadius
height = 27 * manPacRadius

-- How big manPac is
manPacRadius :: Double
manPacRadius = 15

-- How fast is manPac
manPacSpeed :: Double
manPacSpeed = 5

-- | Move a point by a certain velocity.
move :: Point -> Vector -> Point
move (x, y) (xv, yv) = (x + xv, y + yv)

-- | Ensure that the point is inside the rectangle.
clamp :: Point -> Rect -> Point
clamp (x, y) (Rect xMin yMin xMax yMax) =
  (min (max x xMin) xMax, min (max y yMin) yMax)

-- | Is the point inside the rectangle?
inside :: Point -> Rect -> Bool
inside (x, y) (Rect x1 y1 x2 y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

-- Are the rectangles overlapping?
overlaps :: Rect -> Rect -> Bool
overlaps (Rect r1x1 r1y1 r1x2 r1y2) (Rect r2x1 r2y1 r2x2 r2y2) = 
	((r1x1 < (r2x1+r2x2)) &&
		((r1x1+r1x2) > r2x1) &&
		(r1y1 < (r2y1+r2y2)) &&
		((r1y2+r1y2) > r2y1))

-- | Update the ball's position with its velocity.
moveManPac :: GameState -> GameState
moveManPac state = case or [overlaps (manPacBox p) x | x <- (wallBlocks state)] of
						True  -> state {manPacDir = (0,0)}
						False -> state {manPacPos = p}
 where
    playingField = Rect manPacRadius manPacRadius (width - manPacRadius) (height - manPacRadius)
    p = ((manPacPos state) `move` (manPacDir state)) `clamp` playingField

manPacBox :: Point -> Rect
manPacBox (px,py) = Rect (px - r) (py - r) (r * 2) (r * 2)
	where r = manPacRadius

-- | Update the paddles depending on the currently pressed keys.
changeManPacDir :: S.Set Char -> GameState -> GameState
changeManPacDir keys state = pacDir 'W' 'S' 'A' 'D' 
  where
    pacDir up down left right
      | (up `S.member` keys)   && invalidDir state (0, -manPacSpeed) = state { manPacDir = (0, -manPacSpeed) }
      | (down `S.member` keys) && invalidDir state (0, manPacSpeed)  = state { manPacDir = (0, manPacSpeed)  }
      | (left `S.member` keys) && invalidDir state (-manPacSpeed, 0) = state { manPacDir = (-manPacSpeed, 0) }
      | (right `S.member`keys) && invalidDir state (manPacSpeed, 0)  = state { manPacDir = (manPacSpeed, 0)  }
      | otherwise            = state

invalidDir :: GameState -> Vector -> Bool
invalidDir state v = not $ invalidDir' ((manPacPos state) `move` v) 
	|| invalidDir'' ((manPacPos state) `move` (manPacDir state)) state

invalidDir' :: Point -> Bool
invalidDir' (x,y) = (x+manPacRadius) > width || (x-manPacRadius) < 0 
					|| (y+manPacRadius) > height || (y-manPacRadius) < 0

invalidDir'' :: Point -> GameState -> Bool
invalidDir'' p state = or [overlaps (manPacBox p) x | x <- (wallBlocks state)]

initialState :: GameState
initialState = GameState {
	manPacPos = (width/2, manPacRadius*23),
    manPacDir = (0,0),
    wallBlocks = walls
}

walls :: [Rect]
walls = [Rect (manPacRadius*2 + p*(manPacRadius*15)) (manPacRadius*2) (manPacRadius*13) manPacRadius | p <- [0..1] ]
		++ [Rect (manPacRadius*2 + p*(manPacRadius*3)) (manPacRadius*5) manPacRadius (manPacRadius*17) | p <- [0..9] ] 
		++ [Rect (manPacRadius*2 + p*(manPacRadius*15)) (manPacRadius*24) (manPacRadius*13) manPacRadius | p <- [0..1] ]
		-- ++ [Rect (manPacRadius*2 + p*(manPacRadius*3)) (manPacRadius*27) manPacRadius (manPacRadius*17) | p <- [0..25] ] 
		-- ++ [Rect (manPacRadius*2 + p*(manPacRadius*15)) (manPacRadius*46) (manPacRadius*13) manPacRadius | p <- [0..4] ]

