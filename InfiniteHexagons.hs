import Control.Applicative ((<$>), (<*>))
import Data.Fixed (mod')
import Data.List (sort)
import Data.IORef
import Graphics.UI.GLUT
import Local.GLUTHelpers (ColoredPolygonT, GL3T, drawScaledPolygonFT)

type GL2T = (GLfloat, GLfloat)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _window <- createWindow "Imagine an infinite plane of hexagons"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    size <- get windowSize
    fillWithHexagons size 60
    flush
  where
    hex :: ColoredPolygonT
    hex = let r = 60
      in ((phiSinebow 1), map (\phi -> (r * (sin phi), r * (cos phi), 0)) [n * pi / 3 | n <- [0..5]])

hexToCartesian :: GLfloat                   -- the unit-radius for a hexagon
               -> GL2T                      -- the point's (hexX, hexY)
               -> GL2T                      -- the cartesian (x, y)
hexToCartesian radius (hexX, hexY) = let
    shortRadius = radius * 3 ** 0.5 / 2
    y :: GLfloat
    y = shortRadius * (2 * hexY + hexX)
    x :: GLfloat
    x = 3 / 2 * radius * hexX
  in (x, y)

fillWithHexagons :: Size -> GLfloat -> IO ()
fillWithHexagons (Size width height) radius = do
    mapM_ drawHexagonAt coordinatesList
  where
    -- at the origin, calculate the maximum x and y
    xMaxOrigin = ceiling ((fromIntegral width) / (radius * 3 / 2))
    --xRange = [(negate xMaxOrigin)..xMaxOrigin]
--    xRange = [0..2]
    hexSize = (5, 11)
    xMax = ceiling ((fromIntegral (fst hexSize) - 1) / 2)
    yMax = ceiling ((fromIntegral (snd hexSize) - 1) / 2)
    xRange = [(negate xMax)..xMax]
    ySize = 2 * yMax
    coordinatesList :: [GL2T]
    coordinatesList = xRange >>= getColumn
    getColumn :: Integral a => a              -- the column number (hexX)
              -> [GL2T]                       -- a list of (hexX, hexY) values for that column
    getColumn x = let
        yMin = (negate (yMax + ((x + 1) `div` 2)))
        localYMax = yMin + (ySize + (x `mod` 2))
        yRange = [yMin..localYMax]
      in [(fromIntegral x, fromIntegral y) | y <- yRange]
    drawHexagonAt :: GL2T                     -- the (hexX, hexY) coordinates at which to draw a hex
                  -> IO ()                    -- the rendered hexagon
    drawHexagonAt (hexX, hexY) = let
        (x, y) = hexToCartesian radius (hexX, hexY)
      in
        drawScaledPolygonFT (Size width height) $ (phiSinebow (floor (hexX + 3 * hexY)), hexagonVerticesFor (x, y))
      where
        hexagonVerticesFor :: GL2T            -- the cartesian (x, y) coordinates
                           -> [GL3T]          -- the list of 3-tuple (x, y, z) coordinates for Vertex3
        hexagonVerticesFor (x, y) = map (\phi -> (x + radius * (cos phi), y + radius * (sin phi), 0)) [n * pi / 3 | n <- [0..5]]

sinebow :: GLfloat      -- a float to convert to a sinebow color
        -> GL3T         -- a 3-tuple of color. See http://basecase.org/env/on-rainbows
sinebow n = (red, green, blue)
  where
    scaling = pi
    red = (sin (n * scaling)) ** 2
    green = (sin ((n + 1 / 3) * scaling)) ** 2
    blue = (sin ((n + 2 / 3) * scaling)) ** 2

phiSinebow :: Integral a => a   -- an integral number to choose the nth color from the sinebow
           -> GL3T              -- the nth color from the sinebow. See same as `sinebow`
phiSinebow n = let phi = 1 + 2 ** 0.5
  in sinebow (fromIntegral n * phi)
