module Logic where
import qualified Data.Set as S
import Data.List
import Haste.Graphics.Canvas

-- The state of our game.
data GameState = GameState {
    manPacPos :: Point,
    manPacDir :: Vector,
    ghostPos :: Point,
    ghostDir :: Vector,
    ghost2Pos :: Point,
    ghost2Dir :: Vector,
    wallBlocks :: [Rect],
    pellets :: [Point],
    score :: Int,
    tilemap :: Tilemap,
    animations :: [Animation],
    activeA :: Animation
  }

data Tilemap = Tilemap {
    bitmap    :: Bitmap,
    mapH :: Double,
    mapW :: Double
}

data Animation = Animation {
	tiles    :: [Rect],
	timing   :: [Integer],
	counter :: Integer
}

-- Width and height of the playing field.
width, height :: Double
width  = 30 * manRad
height = 22 * manRad

-- How big manPac is.
-- All measures in the whole game are based on manPacs size.
-- This is because is makes it easier to place everything in the map.
manRad :: Double
manRad = 20

-- How big pellets are
pelletRad :: Double
pelletRad = manRad/5

-- How fast is manPac
manPacSpeed :: Double
manPacSpeed = manRad / 4

-- Move a point by a certain velocity vector.
move :: Point -> Vector -> Point
move (x, y) (xv, yv) = (x + xv, y + yv)

-- Check that a point is inside a rectangle.
clamp :: Point -> Rect -> Point
clamp (x, y) (Rect xMin yMin xMax yMax) =
  (min (max x xMin) xMax, min (max y yMin) yMax)

-- Checks if a point is inside a rectangle.
inside :: Point -> Rect -> Bool
inside (x, y) (Rect x1 y1 x2 y2) =
  x >= x1 && x <= (x1+x2) && y >= y1 && y <= (y1+y2)

-- Checks if two rectangles are overlapping
overlaps :: Rect -> Rect -> Bool
overlaps (Rect r1x1 r1y1 r1w r1l) (Rect r2x1 r2y1 r2w r2l) = 
	((r1x1 < (r2x1+r2w)) &&
	((r1x1+r1w) > r2x1) &&
	(r1y1 < (r2y1+r2l)) &&
	((r1y1+r1l) > r2y1))

-- Updates pacmans position with his velocity vector.
moveManPac :: GameState -> GameState
moveManPac state = case or [overlaps (circleToBox p manRad) x | x <- (wallBlocks state)] of
						True  -> state {manPacDir = (0,0)}
						False -> state {manPacPos = p}
 where
    p = ((manPacPos state) `move` (manPacDir state))

-- Moves the black ghost, it is supposed to always go towards the manPac.
-- It is very basic homing and far from perfect, it chooses direction based on 
-- the difference in x and y between its own and manPacs position. 
-- Where it prioritizes movement in x direction.
moveHomingGhost :: GameState -> GameState
moveHomingGhost state = case or [overlaps (circleToBox p manRad) x | x <- (wallBlocks state)] of
					   		True  -> state 
					   		False -> state { ghost2Pos = p, ghost2Dir = dir }
  where
  	xDiff = pointXDiff (ghost2Pos state) (manPacPos state)
  	yDiff = pointYDiff (ghost2Pos state) (manPacPos state) 
  	p = (ghost2Pos state) `move` dir
  	dir = newDir
  	newDir 
  	  | xDiff < 0 && invalidDir state (ghost2Pos state) (manPacSpeed, 0) = (manPacSpeed, 0) 
	  | xDiff > 0 && invalidDir state (ghost2Pos state) (-manPacSpeed, 0) = (-manPacSpeed, 0)
	  | yDiff < 0 && invalidDir state (ghost2Pos state) (0, manPacSpeed) = (0, manPacSpeed)
	  | yDiff > 0 && invalidDir state (ghost2Pos state) (0, -manPacSpeed) = (0, -manPacSpeed) 
	  | otherwise = (ghost2Dir state)

-- Calculates the difference in x between two points
pointXDiff :: Point -> Point -> Double
pointXDiff (x1,y1) (x2,y2) = (x1-x2) 

-- Calculates the difference in y between two points
pointYDiff :: Point -> Point -> Double
pointYDiff (x1,y1) (x2,y2) = (y1-y2) 

-- Moves the first "random" ghost (the white one), it is supposed to go until it hits a wall and then turn in another legal direction 
-- based on which direction it was traveling previously acording to the pattern (up -> left -> down -> right)
-- If it can't find a new direction it gets a new direction from stepVector and calculates a new direction from that.
-- The different vectors are: (0, -speed) = up, (0, speed) = down, (-speed, 0) = left, (speed, 0) = right.
-- The ghost tends to get stuck in loop, but sometimes if you restart or die he goes on another course for reasons unknown.
moveGhost :: GameState -> GameState
moveGhost state = case or [overlaps (circleToBox p manRad) x | x <- (wallBlocks state)] of
					   True  -> state { ghostPos = newPos, ghostDir = newDir dir }
					   False -> state { ghostPos = p }
  where
  	p = ((ghostPos state) `move` (ghostDir state))
  	dir = ghostDir state
  	newDir currDir
	   | (currDir == (0, -manPacSpeed)) && invalidDir state (ghostPos state) (-manPacSpeed, 0) = (-manPacSpeed, 0)
	   | (currDir == (-manPacSpeed, 0)) && invalidDir state (ghostPos state) (0, manPacSpeed) = (0, manPacSpeed)
	   | (currDir == (0, manPacSpeed)) && invalidDir state (ghostPos state) (manPacSpeed, 0) = (manPacSpeed, 0)
	   | (currDir == (manPacSpeed, 0)) && invalidDir state (ghostPos state) (0, -manPacSpeed) = (0, -manPacSpeed)
	   | otherwise = newDir (stepVector currDir)
  	newPos = ((ghostPos state) `move` newDir dir)

-- Returns a new direction vector based on the current vector 
-- Up = down, down = left, left = right, right = up
stepVector :: Vector -> Vector
stepVector vec | vec == (0, -manPacSpeed) = (0, manPacSpeed)
			   | vec == (0, manPacSpeed) = (-manPacSpeed, 0)
			   | vec == (-manPacSpeed, 0) = (manPacSpeed, 0)
			   | vec == (manPacSpeed, 0) = (0, -manPacSpeed)

-- Calculates the square of a position based on its radius.
circleToBox :: Point -> Double -> Rect
circleToBox (px,py) r = Rect (px - r) (py - r) (r * 2) (r * 2)

-- Increments the animation counter, puts manPac in the next step of the animation.
incAnim :: GameState -> GameState
incAnim state | (manPacDir state) == (0,0) = state {activeA = (activeA state) {counter = c' - 1}}
			  | c' >= last (timing (activeA state)) = state {activeA = (activeA state) {counter = 0}}
			  | otherwise                          = state {activeA = (activeA state) {counter = c'}}
			 where c' = (counter (activeA state)) + 1

-- Updates manPac direction depending on the currently pressed keys, except if there is a wall in that direction.
changeManPacDir :: S.Set Char -> GameState -> GameState
changeManPacDir keys state = (pacDir 'W' 'S' 'A' 'D')
  where
    pacDir up down left right
      | (up `S.member` keys)   && invalidDir state (manPacPos state) (0, -manPacSpeed) = turnPac state (0, -manPacSpeed)
      | (down `S.member` keys) && invalidDir state (manPacPos state) (0, manPacSpeed)  = turnPac state (0, manPacSpeed)
      | (left `S.member` keys) && invalidDir state (manPacPos state) (-manPacSpeed, 0) = turnPac state (-manPacSpeed, 0)
      | (right `S.member`keys) && invalidDir state (manPacPos state) (manPacSpeed, 0)  = turnPac state (manPacSpeed, 0)
      | otherwise            = state

-- Checks if you turn in a direction manPac is already moving, so it doesn't reset the animation.
turnPac :: GameState -> Point -> GameState
turnPac state (dirX,dirY) | dirX == 0 && dirY > 0 = state {activeA = (animations state) !! 2, manPacDir = (dirX,dirY)}
						  | dirX == 0 && dirY < 0 = state {activeA = (animations state) !! 1, manPacDir = (dirX,dirY)}
						  | dirX > 0 && dirY == 0 = state {activeA = (animations state) !! 3, manPacDir = (dirX,dirY)}
						  | otherwise = state {activeA = (animations state) !! 0, manPacDir = (dirX,dirY)}

-- Checks if a pellet is within manPacs hitbox,
-- if so it removes the pellet from the list of pellets and increments the score by one.
pelletCollide :: GameState -> GameState
pelletCollide state = pelletCollide' [ p | p <- (pellets state),p `inside` (circleToBox (manPacPos state) (manRad/2))] state

-- Help Method for pelletCollide, here the actual pellet is delted and the score is incremented.
-- It also covers the case if there would be several pellets within manPacs radius, which can't currently happend.
pelletCollide' :: [Point] -> GameState -> GameState
pelletCollide' [] state = state 
pelletCollide' (x:xs) state = pelletCollide' xs state { pellets = delete x (pellets state), 
									  score = (score state) + 1 }  

-- If manPac is outside, he is teleported to the other side.
checkBounding :: GameState -> GameState
checkBounding state = state { manPacPos = (oOB (manPacPos state)), ghostPos = (oOB (ghostPos state))}
	where 
		oOB :: Point -> Point
		oOB (x,y)
			| x > width = (0,y)
			| x < 0	    = (width,y)
			| otherwise = (x,y)

-- Checks if a point can move with a vector.
invalidDir :: GameState -> Point -> Vector -> Bool
invalidDir state pos v =  (not (v == (manPacDir state))) && (not $ invalidDir' (pos `move` v) state)

-- Checks if a point is inside one of the walls.
invalidDir' :: Point -> GameState -> Bool
invalidDir' p state = or [overlaps (circleToBox p manRad) x | x <- (wallBlocks state)]

-- Checks whether the game is over or not.
gameOver :: GameState -> Bool
gameOver state = ((circleToBox (ghostPos state) manRad) `overlaps` (circleToBox (manPacPos state) manRad)) || 
				 ((circleToBox (ghost2Pos state) manRad) `overlaps` (circleToBox (manPacPos state) manRad)) ||
				  (pellets state) == []

-- The initial state for the game, needs resources to create.
initialState :: Tilemap -> [Animation] -> GameState
initialState tile anims = GameState {
	manPacPos = (width/2, manRad*6),
    manPacDir = (0,0),
    ghostPos = (width/2, manRad*18),
    ghostDir = (0, -manPacSpeed),
    ghost2Pos = (width/2, manRad*18),
    ghost2Dir = (0, -manPacSpeed),
    wallBlocks = walls,
    pellets = pelletsInit,
    score = 0,
    tilemap = tile,
    animations = anims,
    activeA = head anims
}

-- The list of all pellet points.
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

-- The rectangles for the walls.
walls :: [Rect]
walls = -- the outer walls
		[Rect 0 0 (manRad*30) manRad]  ++ 
		[Rect 0 0 manRad (manRad*9)]  ++
		[Rect 0 (manRad*11) manRad (manRad*8)]  ++
		[Rect (manRad*29) 0 manRad (manRad*9)]  ++
		[Rect (manRad*29) (manRad*11) manRad (manRad*8)]  ++
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
		[Rect (manRad*12) (manRad*15) (manRad*2) (manRad*1) ] ++
		[Rect (manRad*16) (manRad*15) (manRad*2) (manRad*1) ] ++
		[Rect (manRad*12) (manRad*16) (manRad*1) (manRad*4) ] ++ 
		[Rect (manRad*17) (manRad*16) (manRad*1) (manRad*4) ]
