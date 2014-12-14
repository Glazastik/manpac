import Haste
import Haste.Graphics.Canvas
import qualified Data.Set as S
import Data.IORef
import Data.Char
import Logic

data Animation = Animation {
	pics :: [Bitmap],
	timing :: [Integer],
	pos :: Point,
	loaded :: Bool
}


-- | Render the game's state to a canvas.
renderState :: Canvas -> GameState -> IO ()
renderState can state = render can $ do
	manPac (manPacPos state)
	ghostPic (ghostPos state)
	mapM_ wallPic $ (wallBlocks state) 
	mapM_ pellet $ (pellets state)

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
    setScore (score state)
    renderState can state'
    setTimeout 30 $ tick can keysRef state'
  where
    update keys = checkBounding . pelletCollide . moveManPac . changeManPacDir keys

-- | Create a new scoreboard.
newScoreboard :: IO Elem
newScoreboard = do
  scoreboard <- newElem "div"
  setStyle scoreboard "margin" "auto"
  setStyle scoreboard "width" "150px"

  score <- newElem "div"
  setProp score "id" ("Score")

  setChildren scoreboard [score]
  return scoreboard

main :: IO ()
main = do
  canvasElem <- newCanvas width height
  Just canvas <- getCanvas canvasElem
  scoreboard <- newScoreboard
  setChildren documentBody [canvasElem, scoreboard]
  setScore 0
  --render canvas $ do manPac (height/2,width/2)



  --pacImgR <- newIORef $ manPacImg

  keysPressed <- newIORef S.empty
  documentBody `onEvent` OnKeyDown $ \k -> do
    atomicModifyIORef keysPressed $ \keys -> (S.insert (chr k) keys, ())
  documentBody `onEvent` OnKeyUp $ \k -> do
    atomicModifyIORef keysPressed $ \keys -> (S.delete (chr k) keys, ())
  --bitmapElem manPacImg `onEvent` OnLoad $ do
    --drawImg manPacImg canvas (150,150)

  tick canvas keysPressed initialState

drawImg :: Bitmap -> Canvas -> Point -> IO ()
drawImg img c pt = do
  render c $ scale (1,1) $ do
  	draw img pt

-- Update the scoreboard.
setScore :: Int -> IO ()
setScore score = withElem ("Score") $ \e -> do
  setProp e "innerText" ("score: " ++ show score)

manPac :: Point -> Picture () 
manPac pt = color (RGB 255 255 0) $ do
  fill $ circle pt manRad 

ghostPic :: Point -> Picture ()
ghostPic pt = color (RGB 0 0 255) $ do 
	fill $ circle pt manRad

wallPic :: Rect -> Picture ()
wallPic (Rect x1 y1 x2 y2) = color (RGB 80 80 255) $ do
  fill $ rect (x1,y1) (x1+x2,y1+y2) 

pellet :: Point -> Picture ()
pellet pt = color (RGB 255 255 255) $ do
	fill $ circle pt pelletRad