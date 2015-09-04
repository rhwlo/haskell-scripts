import Data.List (foldl')
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import Graphics.UI.GLUT
import Local.GLUTHelpers (GL3T, drawScaledPolygonFT)

data HexCoordinate = HexC GLfloat GLfloat deriving (Show, Eq)
instance Monoid HexCoordinate where
  mempty = HexC 0 0
  mappend (HexC x y) (HexC x' y') = HexC (x + x') (y + y')
  mconcat = foldl' mappend mempty

data CartCoordinate = CartC GLfloat GLfloat deriving (Show, Eq)
instance Monoid CartCoordinate where
  mempty = CartC 0 0
  mappend (CartC x y) (CartC x' y') = CartC (x + x') (y + y')
  mconcat = foldl' mappend mempty

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

hexToCartesian :: GLfloat                   -- the unit-radius for a hexagon
               -> HexCoordinate             -- the hexagonal coordinate to convert
               -> CartCoordinate            -- the cartesian coordinate resulting
hexToCartesian radius (HexC hexX hexY) = let
    shortRadius = radius * 3 ** 0.5 / 2
    y :: GLfloat
    y = shortRadius * (2 * hexY + hexX)
    x :: GLfloat
    x = 3 / 2 * radius * hexX
  in CartC x y

fillWithHexagons :: Size -> GLfloat -> IO ()
fillWithHexagons (Size width height) radius = do
    mapM_ drawHexagonAt hexCoordinateList
  where
    yPackingRadius :: GLfloat
    yPackingRadius = radius * sqrt(3) / 2
    xPackingRadius :: GLfloat
    xPackingRadius = radius * 3/4
    yOriginMax = ((fromIntegral height) - yPackingRadius) / (2 * yPackingRadius)
    ySize = yOriginMax * 2
    xMax = ((fromIntegral width) - xPackingRadius) / (2 * xPackingRadius)
    xRange :: [Int]
    xRange = [(negate (ceiling xMax))..(ceiling xMax)]
    hexCoordinateList :: [HexCoordinate]
    hexCoordinateList = xRange >>= \x -> let
        yMin = negate (ceiling yOriginMax + (div x 2) + (mod x 2))
        yMax = yMin + ceiling ySize + (mod x 2)
        yRange = [yMin..yMax]
      in [HexC (fromIntegral x) (fromIntegral y) | y <- yRange ]
    drawHexagonAt :: HexCoordinate            -- the (hexX, hexY) coordinates at which to draw a hex
                  -> IO ()                    -- the rendered hexagon
    drawHexagonAt hexCoordinate = let
        (HexC hexX hexY) = hexCoordinate
        cartCoordinate = hexToCartesian radius hexCoordinate
      in
        drawScaledPolygonFT (Size width height) $ (phiSinebow (floor (hexX + 3 * hexY)), hexagonVerticesFor cartCoordinate)
      where
        hexagonVerticesFor :: CartCoordinate  -- the cartesian (x, y) coordinates
                           -> [GL3T]          -- the list of 3-tuple (x, y, z) coordinates for Vertex3
        hexagonVerticesFor (CartC x y) = map (\phi -> (x + radius * (cos phi), y + radius * (sin phi), 0)) [n * pi / 3 | n <- [0..5]]

sinebow :: GLfloat      -- a float to convert to a sinebow color
        -> GL3T         -- a 3-tuple of color. See http://basecase.org/env/on-rainbows
sinebow n = (red, green, blue)
  where
    scaling = pi
    red = (sin (n * scaling)) ** 2
    green = (sin ((n + 1 / 3) * scaling)) ** 2
    blue = (sin ((n + 2 / 3) * scaling)) ** 2

phiSinebow :: Integer           -- an integer to choose the nth color from the sinebow
           -> GL3T              -- the nth color from the sinebow. See same as `sinebow`
phiSinebow n = let phi = 1 + 2 ** 0.5
  in sinebow (fromIntegral n * phi)
