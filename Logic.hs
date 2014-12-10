module Logic where
import qualified Data.Set as S
import Data.List
import Haste.Graphics.Canvas

-- | The state of our game.
data GameState = GameState {
    manPacPos :: Point,
    manPacDir :: Vector,
    wallBlocks :: [Rect],
    pellets :: [Point],
    score :: Int
  }

type Wall = Rect

-- Width and height of the playing field.
width, height :: Double
width  = 30 * manRad
height = 21 * manRad

-- How big manPac is
manRad :: Double
manRad = 20

-- How big pellets are
pelletRad :: Double
pelletRad = manRad/5

-- How fast is manPac
manPacSpeed :: Double
manPacSpeed = manRad / 4

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
overlaps (Rect r1x1 r1y1 r1w r1l) (Rect r2x1 r2y1 r2w r2l) = 
	((r1x1 < (r2x1+r2w)) &&
		((r1x1+r1w) > r2x1) &&
		(r1y1 < (r2y1+r2l)) &&
		((r1y1+r1l) > r2y1))

-- | Update the ball's position with its velocity.
moveManPac :: GameState -> GameState
moveManPac state = case or [overlaps (manPacBox p) x | x <- (wallBlocks state)] of
						True  -> state {manPacDir = (0,0)}
						False -> state {manPacPos = p}
 where
    playingField = Rect manRad manRad (width - manRad) (height - manRad)
    p = ((manPacPos state) `move` (manPacDir state)) `clamp` playingField

manPacBox :: Point -> Rect
manPacBox (px,py) = Rect (px - r) (py - r) (r * 2) (r * 2)
	where r = manRad

-- | Updates pacman direction depending on the currently pressed keys, except if there is a wall in that direction.
changeManPacDir :: S.Set Char -> GameState -> GameState
changeManPacDir keys state = pacDir 'W' 'S' 'A' 'D' 
  where
    pacDir up down left right
      | (up `S.member` keys)   && invalidDir state (0, -manPacSpeed) = state { manPacDir = (0, -manPacSpeed) }
      | (down `S.member` keys) && invalidDir state (0, manPacSpeed)  = state { manPacDir = (0, manPacSpeed)  }
      | (left `S.member` keys) && invalidDir state (-manPacSpeed, 0) = state { manPacDir = (-manPacSpeed, 0) }
      | (right `S.member`keys) && invalidDir state (manPacSpeed, 0)  = state { manPacDir = (manPacSpeed, 0)  }
      | otherwise            = state

pelletCollide :: GameState -> GameState
pelletCollide state = pelletCollide' [ p | p <- (pellets state),p `inside` (manPacBox (manPacPos state))] state

pelletCollide' :: [Point] -> GameState -> GameState
pelletCollide' [] state = state 
pelletCollide' [x] state = state { pellets = delete x (pellets state), 
									  score = (score state) + 1 } 

invalidDir :: GameState -> Vector -> Bool
invalidDir state v = not $ invalidDir' ((manPacPos state) `move` v) 
	|| invalidDir'' ((manPacPos state) `move` v) state

invalidDir' :: Point -> Bool
invalidDir' (x,y) = (x+manRad) > width || (x-manRad) < 0 
					|| (y+manRad) > height || (y-manRad) < 0

invalidDir'' :: Point -> GameState -> Bool
invalidDir'' p state = or [overlaps (manPacBox p) x | x <- (wallBlocks state)]

initialState :: GameState
initialState = GameState {
	manPacPos = (width/2, manRad*14),
    manPacDir = (0,0),
    wallBlocks = walls,
    pellets = [(manRad,manRad), (manRad*3, manRad), (manRad*5, manRad), (manRad*7, manRad), (manRad*9, manRad), (manRad*11, manRad), (manRad*13, manRad)],
    score = 0
}

walls :: [Rect]
walls = [Rect 0 0 (manRad*30) manRad]  ++ 
		[Rect 0 0 manRad (manRad*18)]  ++
		[Rect (manRad*29) 0 manRad (manRad*18)]  ++
		[Rect 0 (manRad*18) (manRad*30) manRad]  ++

		[Rect (manRad*14) (manRad*1) (manRad*2) (manRad*4) ] ++
		[Rect (manRad*14) (manRad*7) (manRad*2) (manRad*4) ] ++
		[Rect (manRad*12) (manRad*11) (manRad*6) (manRad*2) ] ++

		[Rect (manRad*3) (manRad*3) (manRad*4) (manRad*2) ] ++
		[Rect (manRad*9) (manRad*3) (manRad*3) (manRad*2) ] ++ 
		[Rect (manRad*18) (manRad*3) (manRad*3) (manRad*2) ] ++
		[Rect (manRad*23) (manRad*3) (manRad*4) (manRad*2) ] ++

		[Rect (manRad*3) (manRad*7) (manRad*9) (manRad*2) ] ++
		[Rect (manRad*5) (manRad*9) (manRad*2) (manRad*2) ] ++
		[Rect (manRad*18) (manRad*7) (manRad*9) (manRad*2) ] ++
		[Rect (manRad*23) (manRad*9) (manRad*2) (manRad*2) ] 
{-
walls :: [Rect]
walls = [Rect (manRad*2 + p*(manRad*15)) (manRad*2) (manRad*13) manRad | p <- [0..1] ]
		++ [Rect (manRad*2 + p*(manRad*3)) (manRad*5) manRad (manRad*17) | p <- [0..9] ] 
		++ [Rect (manRad*2 + p*(manRad*15)) (manRad*24) (manRad*13) manRad | p <- [0..1] ]
		-- ++ [Rect (manRad*2 + p*(manRad*3)) (manRad*27) manRad (manRad*17) | p <- [0..25] ] 
		-- ++ [Rect (manRad*2 + p*(manRad*15)) (manRad*46) (manRad*13) manRad | p <- [0..4] ]
-}
