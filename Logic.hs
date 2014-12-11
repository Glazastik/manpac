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
height = 22 * manRad

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
  x >= x1 && x <= (x1+x2) && y >= y1 && y <= (y1+y2)

-- Are the rectangles overlapping?
overlaps :: Rect -> Rect -> Bool
overlaps (Rect r1x1 r1y1 r1w r1l) (Rect r2x1 r2y1 r2w r2l) = 
	((r1x1 < (r2x1+r2w)) &&
		((r1x1+r1w) > r2x1) &&
		(r1y1 < (r2y1+r2l)) &&
		((r1y1+r1l) > r2y1))

-- | Update the ball's position with its velocity.
moveManPac :: GameState -> GameState
moveManPac state = case or [overlaps (circleToBox p manRad) x | x <- (wallBlocks state)] of
						True  -> state {manPacDir = (0,0)}
						False -> state {manPacPos = p}
 where
    playingField = Rect manRad manRad (width - manRad) (height - manRad)
    p = ((manPacPos state) `move` (manPacDir state)) `clamp` playingField

circleToBox :: Point -> Double -> Rect
circleToBox (px,py) r = Rect (px - r) (py - r) (r * 2) (r * 2)

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
pelletCollide state = pelletCollide' [ p | p <- (pellets state),p `inside` (circleToBox (manPacPos state) (manRad/2))] state

pelletCollide' :: [Point] -> GameState -> GameState
pelletCollide' [] state = state 
pelletCollide' (x:xs) state = pelletCollide' xs state { pellets = delete x (pellets state), 
									  score = (score state) + 1 }  

invalidDir :: GameState -> Vector -> Bool
invalidDir state v = not $ invalidDir' ((manPacPos state) `move` v) 
	|| invalidDir'' ((manPacPos state) `move` v) state

invalidDir' :: Point -> Bool
invalidDir' (x,y) = (x+manRad) > width || (x-manRad) < 0 
					|| (y+manRad) > height || (y-manRad) < 0

invalidDir'' :: Point -> GameState -> Bool
invalidDir'' p state = or [overlaps (circleToBox p manRad) x | x <- (wallBlocks state)]

initialState :: GameState
initialState = GameState {
	manPacPos = (width/2, manRad*6),
    manPacDir = (0,0),
    wallBlocks = walls,
    pellets = pelletsInit,
    score = 0
}

pelletsInit :: [Point]
pelletsInit = --Top row
			  [(manRad*(p+2),manRad*2) | p <- [1..10]] ++ [(manRad*(p+17),manRad*2) | p <- [1..10]] ++
			  --Second row
			  [(manRad*(p+2),manRad*6) | p <- [1..10]] ++ [(manRad*(p+17),manRad*6) | p <- [1..10]]	++
			  --Inner columns
			  [(manRad*13,manRad*(p+1)) | p <- [1..8]] ++ [(manRad*17,manRad*(p+1)) | p <- [1..8]]	++
			  --Inner lower column
			  [(manRad*11,manRad*(p+10)) | p <- [1..8]] ++ [(manRad*19,manRad*(p+10)) | p <- [1..8]]	++
			  --Long outer columns
			  [(manRad*2,manRad*(p+1)) | p <- [1..9]] ++ [(manRad*28,manRad*(p+1)) | p <- [1..9]]	++
			  --Short outer columns
			  [(manRad*2,manRad*(p+13)) | p <- [1..5]] ++ [(manRad*28,manRad*(p+13)) | p <- [1..5]]	++
			  --Small fill columns
			  [(manRad*8,manRad*(p+2)) | p <- [1..3]] ++ [(manRad*22,manRad*(p+2)) | p <- [1..3]]	++
			  --Lower Small fill columns
			  [(manRad*7,manRad*(p+9)) | p <- [1..5]] ++ [(manRad*23,manRad*(p+9)) | p <- [1..5]]	++
			  --Short third row
			  [(manRad*(p+2),manRad*10) | p <- [1..4]] ++ [(manRad*(p+23),manRad*10) | p <- [1..4]] ++
			  --Short fill row
			  [(manRad*(p+10),manRad*10) | p <- [1..3]] ++ [(manRad*(p+16),manRad*10) | p <- [1..3]] ++
			  --Short centered fourth row
			  [(manRad*(p+7),manRad*12) | p <- [1..3]] ++ [(manRad*(p+19),manRad*12) | p <- [1..3]] ++  
			  --Short fifth row
			  [(manRad*(p+2),manRad*14) | p <- [1..4]] ++ [(manRad*(p+23),manRad*14) | p <- [1..4]] ++
			  --Long bottom row
			  [(manRad*(p+2),manRad*18) | p <- [1..8]] ++ [(manRad*(p+19),manRad*18) | p <- [1..8]]

walls :: [Rect]
walls = -- the outer walls
		[Rect 0 0 (manRad*30) manRad]  ++ 
		[Rect 0 0 manRad (manRad*19)]  ++
		[Rect (manRad*29) 0 manRad (manRad*19)]  ++
		[Rect 0 (manRad*19) (manRad*30) manRad]  ++

		-- The middle upside down T
		[Rect (manRad*14) (manRad*1) (manRad*2) (manRad*4) ] ++
		[Rect (manRad*14) (manRad*7) (manRad*2) (manRad*4) ] ++
		[Rect (manRad*12) (manRad*11) (manRad*6) (manRad*2) ] ++

		-- the top most "dots"
		[Rect (manRad*3) (manRad*3) (manRad*4) (manRad*2) ] ++
		[Rect (manRad*9) (manRad*3) (manRad*3) (manRad*2) ] ++ 
		[Rect (manRad*18) (manRad*3) (manRad*3) (manRad*2) ] ++
		[Rect (manRad*23) (manRad*3) (manRad*4) (manRad*2) ] ++

		-- The cross-like near the middle
		[Rect (manRad*3) (manRad*7) (manRad*9) (manRad*2) ] ++
		[Rect (manRad*8) (manRad*9) (manRad*2) (manRad*2) ] ++
		[Rect (manRad*18) (manRad*7) (manRad*9) (manRad*2) ] ++
		[Rect (manRad*20) (manRad*9) (manRad*2) (manRad*2) ] ++

		-- middle sideways bars
		[Rect (manRad*1) (manRad*11) (manRad*5) (manRad*2) ] ++
		[Rect (manRad*24) (manRad*11) (manRad*5) (manRad*2) ] ++

		-- the Ls laying down
		[Rect (manRad*8) (manRad*13) (manRad*2) (manRad*2) ] ++
		[Rect (manRad*3) (manRad*15) (manRad*7) (manRad*2) ] ++
		[Rect (manRad*20) (manRad*13) (manRad*2) (manRad*2) ] ++
		[Rect (manRad*20) (manRad*15) (manRad*7) (manRad*2) ] ++

		-- "ghost cage"
		[Rect (manRad*12) (manRad*15) (manRad*6) (manRad*1) ] ++
		[Rect (manRad*12) (manRad*16) (manRad*1) (manRad*4) ] ++ 
		[Rect (manRad*17) (manRad*16) (manRad*1) (manRad*4) ]
{-
walls :: [Rect]
walls = [Rect (manRad*2 + p*(manRad*15)) (manRad*2) (manRad*13) manRad | p <- [0..1] ]
		++ [Rect (manRad*2 + p*(manRad*3)) (manRad*5) manRad (manRad*17) | p <- [0..9] ] 
		++ [Rect (manRad*2 + p*(manRad*15)) (manRad*24) (manRad*13) manRad | p <- [0..1] ]
		-- ++ [Rect (manRad*2 + p*(manRad*3)) (manRad*27) manRad (manRad*17) | p <- [0..25] ] 
		-- ++ [Rect (manRad*2 + p*(manRad*15)) (manRad*46) (manRad*13) manRad | p <- [0..4] ]
-}
