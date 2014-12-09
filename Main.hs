import Haste
import Haste.Graphics.Canvas
import Logic

-- Create a new canvas to draw on.
newCanvas :: Double -> Double -> IO Elem
newCanvas w h = do
	canvas <- newElem "canvas"
	setStyle canvas "border" "2px solid blue"
	setStyle canvas "display" "block"
	setStyle canvas "margin" "auto"
	setStyle canvas "background-color" "black"
	setProp canvas "width" (show w)
	setProp canvas "height" (show h)
	return canvas

main = do
	canvasElem <- newCanvas width height
	Just canvas <- getCanvas canvasElem
	setChildren documentBody [canvasElem]
