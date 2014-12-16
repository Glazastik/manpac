import Haste
import Haste.Graphics.Canvas
import qualified Data.Set as S
import Data.IORef
import Data.Char
import Logic

-- Create a new canvas to draw on.
newCanvas :: Double -> Double -> IO Elem
newCanvas w h = do
	canvas <- newElem "canvas"
	setStyle canvas "border" "2px solid black"
	setStyle canvas "display" "block"
	setStyle canvas "margin" "auto"
	setStyle canvas "background-color" "black"
	setProp canvas "width" (show w)
	setProp canvas "height" (show h)
	return canvas

-- | A unit of time, updates the game state accordingly.
tick :: Canvas -> IORef (S.Set Char) -> GameState -> IO ()
tick can keysRef state = do
    keys <- readIORef keysRef
    let state' = update keys state
    
    renderState can state'
    case gameOver state of
      True -> main
      False -> setTimeout 30 $ tick can keysRef state'
  where
    update keys = moveHomingGhost . incAnim . moveGhost . checkBounding . pelletCollide . moveManPac . changeManPacDir keys


-- | Render the game's state to a canvas.
renderState :: Canvas -> GameState -> IO ()
renderState can state = render can $ do
	mapM_ wallPic $ (wallBlocks state) 
	mapM_ pellet $ (pellets state)
	mapPic (tilemap state) (0,1) (ghostPos state)
	mapPic (tilemap state) (0,2) (ghost2Pos state)
	--drawTile (tilemap state) (1,0) (manPacPos state)
	animatePac (tilemap state) (manPacPos state) (activeA state)
	setScore (score state)

--Main loop of the program.
main :: IO ()
main = do
  canvasElem <- newCanvas width height
  Just canvas <- getCanvas canvasElem
  
  setChildren documentBody [canvasElem]
  render canvas $ text (110, 120) "Loading, please wait..."
  tileset <- loadBitmap "tileset.png" 

  keysPressed <- newIORef S.empty
  documentBody `onEvent` OnKeyDown $ \k -> do
    atomicModifyIORef keysPressed $ \keys -> (S.insert (chr k) keys, ())
  documentBody `onEvent` OnKeyUp $ \k -> do
    atomicModifyIORef keysPressed $ \keys -> (S.delete (chr k) keys, ())
  bitmapElem tileset `onEvent` OnLoad $ do
    initialize canvas keysPressed (newTilemap tileset)
  return ()

--Initialization stuff
initialize :: Canvas -> IORef (S.Set Char) -> Tilemap -> IO ()
initialize c k tmap = tick c k $ initialState tmap (pacAnimations tmap)

-- All the animations for manPac
pacAnimations :: Tilemap -> [Animation]
pacAnimations tilemap = [Animation {
	tiles = [(Rect (x * (mapW tilemap)) 0 (mapW tilemap) (mapH tilemap)) | x <- [3,2,1,0] ],
	timing = [ i*1 | i <- [1..4]],
	counter = 0
}, Animation {
	tiles = [(Rect (x * (mapW tilemap)) 0 (mapW tilemap) (mapH tilemap)) | x <- [6,5,4,0] ],
	timing = [ i*1 | i <- [1..4]],
	counter = 0
}, Animation {
	tiles = [(Rect (x * (mapW tilemap)) (mapH tilemap) (mapW tilemap) (mapH tilemap)) | x <- [3,2,1] ]
		++ [(Rect 0 0 (mapW tilemap) (mapH tilemap))],
	timing = [ i*1 | i <- [1..4]],
	counter = 0
}, Animation {
	tiles = [(Rect (x * (mapW tilemap))  (mapH tilemap) (mapW tilemap) (mapH tilemap)) | x <- [6,5,4] ]
	++ [(Rect 0 0 (mapW tilemap) (mapH tilemap))],
	timing = [ i*1 | i <- [1..4]],
	counter = 0
}]

--Generates a tileMap from a bitmap, magic values are pixel dimensions for each tile
newTilemap :: Bitmap -> Tilemap
newTilemap map1 = Tilemap {
	bitmap = map1,
	mapW = 50,
	mapH = 50
}
	
--Draws the current frame of an animation at the given point.
animate :: Tilemap -> Point -> Animation -> Picture ()
animate tmap pos anim = drawTileRect tmap (getCurrentFrame anim) pos

--Draws a manPac with animation.
animatePac :: Tilemap -> Point -> Animation -> Picture ()
animatePac tmap (x,y) anim = translate (x - manRad ,y - manRad) $
 scale (manRad / mW *2, manRad / mH *2) $ animate tmap (0,0) anim
	where 
  	mH = (mapH tmap)
	mW = (mapW tmap)

--Gets the current frame in an animation as a rectangle.
getCurrentFrame :: Animation -> Rect
getCurrentFrame anim = head [t | (t,x) <- zip (tiles anim) (timing anim), (counter anim) < x]
 
--Draw a rectangle from a tilemap at the given point.
drawTileRect :: Tilemap -> Rect -> Point -> Picture ()
drawTileRect tmap rect pos = drawClipped (bitmap tmap) pos rect

--Draw a tile from a tilemap at the given point.
drawTile :: Tilemap -> Point -> Point -> Picture ()
drawTile tmap (idX,idY) (x,y) = translate (x - manRad ,y - manRad) $ scale (manRad / mW *2, manRad / mH *2) $ 
	drawClipped (bitmap tmap) (0,0) (Rect (idX * mW) (idY * mH) mW mH)
  where 
  	mH = (mapH tmap)
	mW = (mapW tmap)


-- Update the scoreboard.
--setScore :: Int -> IO ()
--setScore score = withElem ("Score") $ \e -> do
--  setProp e "innerText" ("score: " ++ show score)
setScore :: Int -> Picture ()
setScore score = translate (2*manRad, 21.3*manRad) $ scale (2,2) 
	$ color (RGB 255 255 255) $ text (0,0) ("Pellets eaten - " ++ (show score))

--Draws a static picture from the tilemap at the given point.
mapPic :: Tilemap -> Point -> Point -> Picture ()
mapPic tmap tile pos = drawTile tmap tile pos

--Draws a wall at the given rectangle.
wallPic :: Rect -> Picture ()
wallPic (Rect x1 y1 x2 y2) = color (RGB 80 80 255) $ do
  fill $ rect (x1,y1) (x1+x2,y1+y2) 

--Draws a pellet at the given point.
pellet :: Point -> Picture ()
pellet pt = color (RGB 255 255 255) $ do
	fill $ circle pt pelletRad

