module Main where

import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Pages
import Expr



canWidth  = 300
canHeight = 300

-- Function that reads the expression from the given input element and draws the graph on the given canvas
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw input can = do
                          stringExpr <- getProp input "value"
                          case (readExpr stringExpr) of
                            Just expr  -> render can (stroke (path (points expr 0.04 (canWidth, canHeight))))

                            Nothing -> setProp input "value" "Err : Wrong expression"
-- render : Clear a canvas, then draw a picture onto it.
-- stroke : Draw the contours of a shape.

-- Function that reads the expression from the givent input element, differentiate it, show this differentiation
-- in  the input element and draw the graph
showAndDrawDiff :: Elem -> Canvas -> IO()
showAndDrawDiff input can = do
                              stringExpr <- getProp input "value"
                              case (readExpr stringExpr) of
                                Just expr  -> let diffExpr = differentiate expr in do setProp input "value" (showExpr diffExpr)
                                                                                      render can (stroke (path (points diffExpr 0.04 (canWidth, canHeight))))
                                Nothing -> setProp input "value" "Err : Wrong expression"



-- Function that, given an input, a canvas and a scale read the value in the input element and draws the graph on the given canvas with the given scale

zoom :: Elem -> Canvas -> Double -> IO()
zoom input can scale = do
                        stringExpr <- getProp input "value"
                        case (readExpr stringExpr) of
                          Just expr  -> render can (stroke (path (points expr scale (canWidth, canHeight))))
                          Nothing -> setProp input "value" "Err : Wrong expression"

-- Function that save the value of one element as the value of the other

saveExpr :: Elem -> Elem -> IO()
saveExpr input hidden = do
	                        s <- getProp input "value"
	                        -- instead of doing this we wanted to do nothing but we didn't find out how
	                        old <- getProp hidden "value"
	                        case (readExpr s) of 
	                        	Just expr -> setProp hidden "value" s
	                        	otherwise -> setProp hidden "value" old
	                        



{-
zoom :: Elem -> Canvas -> Vector -> Double -> IO()
zoom input can v scale = do
                        stringExpr <- getProp input "value"
                        case (readExpr stringExpr) of
                          Just expr  -> render can (scale v (stroke (path (points expr scale (canWidth, canHeight)))))
                          Nothing -> setProp input "value" "Err : Wrong expression"                         
-}

main = do
    -- Elements
    canvas   <- mkCanvas canWidth canHeight   -- The drawing area
    fx       <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input    <- mkInput 20 "x"                -- The formula input
    draw     <- mkButton "Draw graph"         -- The draw button
    space1   <- mkHTML "<br/>"
    space2   <- mkHTML "<br/>"
    space3   <- mkHTML "<br/>"
    space4   <- mkHTML "<br/>"
    scaleI    <- mkHTML "Scale ="

    zoomScale  <- mkInput 20 "0.04"
    zoomB      <- mkButton "Zoom"
    deZoomB    <- mkButton "Dezoom"

    -- Hidden object that will store the input values in order to zoom even if the input field f(x) is empty
    hidden     <- do 
                     s <- getProp input "value"
    	             newElem "input" `with` [attr "type"  =: "hidden", attr "value" =: s]


    -- Implement a "differentiate" button which displays the differentiated expression and its graph.
    diff     <- mkButton "Differentiate"


      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    formula2 <- mkDiv
    row formula [fx,input]
    row formula2 [scaleI, zoomScale]
    zoomButtons <- mkDiv
    row zoomButtons [zoomB,deZoomB]
    column documentBody [canvas,formula,space1,draw,space2,diff,space3,formula2,space4,zoomButtons,hidden]

    -- Styling
    setStyle documentBody "backgroundColor" "rgb(229,172,182)"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> do saveExpr input hidden 
    	                              readAndDraw input can

    onEvent input KeyUp $ \code -> when (code==13) $  do saveExpr input hidden 
                                                         readAndDraw input can

    onEvent diff Click $ \_ -> do saveExpr input hidden 
    	                          showAndDrawDiff input can
{-
    canvas `onEvent` Click $ \mouse -> do
                                  stringScale <- getProp zoomScale "value"
                                  let (x,y) = mouseCoords mouse
                                  let pos = (150-(fromIntegral x),150-(fromIntegral y))
                                  setProp zoomScale "value" (show (fst pos) ++ " " ++ show (snd pos))
                                  zoom input can (2,2) 0.04--(read stringScale::Double)
-}
    onEvent zoomB Click $ \_ -> do
                                  stringScale <- getProp zoomScale "value"
                                  zoom hidden can (read stringScale::Double)

    onEvent canvas DblClick $ \_ -> zoom hidden can 0.04
    onEvent deZoomB Click $ \_ -> zoom hidden can 0.04

    --   "Enter" key has code 13


-- Function that will calculate all the points of the graph in terms of pixels
-- We assume that our canvas is a square
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = [(fromIntegral x,realToPix(eval exp (pixToReal (fromIntegral x)))) | x<-[0..width] ]
	where
  doubleWidth = fromIntegral width
  realWidth = (scale*doubleWidth)/2
  realRatio =2*realWidth/doubleWidth
  -- converts a pixel x-coordinate to a real x-coordinate
  pixToReal :: Double -> Double
  pixToReal x = (x*realRatio)-realWidth
  -- converts a real y-coordinate to a pixel y-coordinate
  realToPix :: Double -> Double
  realToPix y = (-y+realWidth)/realRatio
