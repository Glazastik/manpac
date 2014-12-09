import Haste
import Haste.Graphics.Canvas
import qualified Data.Set as S
import Data.IORef
import Data.Char
import Logic

-- | Render the game's state to a canvas.
renderState :: Canvas -> GameState -> IO ()
renderState can state = render can $ do
  color (RGB 255 0 0) $ paddle (paddleRect $ leftPaddle state)
  color (RGB 0 0 255) $ paddle (paddleRect $ rightPaddle state)
  ball (ballPos state)

-- | A unit of time, updates the game state accordingly.
tick :: Canvas -> IORef (S.Set Char) -> GameState -> IO ()
tick can keysRef state = do
    keys <- readIORef keysRef
    let state' = update keys state
    renderState can state'
    case checkWinner state' of
      Just West -> do
        setScore West $ leftScore state' + 1
        setTimeout 1000 $ do
          newRound can keysRef $ state' {leftScore = leftScore state' + 1}
      Just East -> do
        setScore East $ rightScore state' + 1
        setTimeout 1000 $ do
          newRound can keysRef $ state' {rightScore = rightScore state' + 1}
      Nothing -> do
        setTimeout 30 $ tick can keysRef state'
  where
    update keys = bouncePaddles . bounceWalls . moveBall . movePaddles keys

-- | Create a new canvas to draw on.
newCanvas :: Double -> Double -> IO Elem
newCanvas w h = do
  canvas <- newElem "canvas"
  setStyle canvas "border" "1px solid black"
  setStyle canvas "display" "block"
  setStyle canvas "margin" "auto"
  setStyle canvas "background-color" "pink"
  setProp canvas "width" (show w)
  setProp canvas "height" (show h)
  return canvas

-- | Draw a paddle.
paddle :: Rect -> Picture ()
paddle (Rect x1 y1 x2 y2) = fill $ rect (x1, y1) (x2, y2)

-- | Draw the ball.
ball :: Point -> Picture ()
ball pt = color (RGB 0 0 127) $ do
  fill $ circle pt ballRadius

-- | Start a new game.
newRound :: Canvas -> IORef (S.Set Char) -> GameState -> IO ()
newRound can keys state = do
  s <- newSeed
  let [xv, yv] = take 2 $ randomRs (-10, 10) s
  tick can keys (state {ballDir = (xv, yv),
                        ballPos = (width/2, height/2)})

-- | Create a new scoreboard.
newScoreboard :: IO Elem
newScoreboard = do
  scoreboard <- newElem "div"
  setStyle scoreboard "margin" "auto"
  setStyle scoreboard "width" "150px"

  leftScore <- newElem "div"
  setProp leftScore "id" (show West)
  
  rightScore <- newElem "div"
  setProp rightScore "id" (show East)

  setChildren scoreboard [leftScore, rightScore]
  return scoreboard

-- | Update the scoreboard for the given player.
setScore :: Player -> Int -> IO ()
setScore player score = withElem (show player) $ \e -> do
  setProp e "innerText" (show player ++ ": " ++ show score)

main = do
  canvasElem <- newCanvas width height
  Just canvas <- getCanvas canvasElem
  scoreboard <- newScoreboard
  setChildren documentBody [canvasElem, scoreboard]

  keysPressed <- newIORef S.empty
  documentBody `onEvent` OnKeyDown $ \k -> do
    atomicModifyIORef keysPressed $ \keys -> (S.insert (chr k) keys, ())
  documentBody `onEvent` OnKeyUp $ \k -> do
    atomicModifyIORef keysPressed $ \keys -> (S.delete (chr k) keys, ())

  setScore West 0
  setScore East 0
  newRound canvas keysPressed initialState