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

showAndDrawDiff :: Elem -> Canvas -> IO()
showAndDrawDiff input can = do
                              stringExpr <- getProp input "value"
                              case (readExpr stringExpr) of
                                Just expr  -> let diffExpr = differentiate expr in do setProp input "value" (showExpr diffExpr)
                                                                                      render can (stroke (path (points diffExpr 0.04 (canWidth, canHeight))))
                                Nothing -> setProp input "value" "Err : Wrong expression"
main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    -- Implement a "differentiate" button which displays the differentiated expression and its graph.
    diff    <- mkButton "Differentiate"

      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw,diff]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
    onEvent diff Click $ \_ -> showAndDrawDiff input can
      -- "Enter" key has code 13


-- type Point = (Double, Double)
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

-- fromIntegral :: Int -> Double