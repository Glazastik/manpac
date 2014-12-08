import Haste
import Haste.Graphics.Canvas
import Logic

-- Create a new canvas to draw on.
newCanvas :: Double -> Double -> IO Elem
newCanvas w h = do
	canvas <- newElem "canvas"
	return canvas

main = do
	canvasElem <- newCanvas width height
	setProp canvas "width" (show w)
    setProp canvas "height" (show h)
	Just canvas <- getCanvas canvasElem