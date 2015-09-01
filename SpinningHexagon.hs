import Data.IORef
import Graphics.UI.GLUT

type GL3Tuple = (GLfloat, GLfloat, GLfloat) -- for clarity's sake

main :: IO ()
main = do
  (_, _) <- getArgsAndInitialize
  _window <- createWindow "hexagons are pretty cool"
  angle <- newIORef 0.0
  direction <- newIORef 1.0
  displayCallback $= display angle
  keyboardMouseCallback $= Just (keyboardMouse direction)
  idleCallback $= Just (idle angle direction)
  mainLoop

keyboardMouse :: IORef GLfloat -> KeyboardMouseCallback
keyboardMouse direction (MouseButton LeftButton) Down _ _ = direction $~! negate
keyboardMouse _ _ _ _ _ = return ()

display :: IORef GLfloat          -- the angle to rotate the hexagon to
        -> DisplayCallback        -- idek, some kind of Haskell magic
display angle = do
    clear [ColorBuffer]
    a <- get angle
    drawPoints a
    flush
  where
    drawPoints :: GLfloat -> IO ()
    drawPoints rotationAngle = do
        mapM_ (renderPrimitive Polygon) (map polygonFromTuples polygons)
      where
        polygons :: [(GL3Tuple, [GL3Tuple])]
        polygons = let phi k = 2 * pi * (k + rotationAngle) / 6
          in
          [ ((1, 0, 0), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [1..2]]),
            ((0.9, 0, 0.3), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [2..3]]),
            ((0.8, 0, 0.4), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [3..4]]),
            ((0.7, 0, 0.5), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [4..5]]),
            ((0.8, 0, 0.4), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [5..6]]),
            ((0.9, 0, 0.3), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [6..7]]) ]

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle wrappedDirection = do
  direction <- get wrappedDirection
  angle $~! (+ (0.001 * direction))
  postRedisplay Nothing

polygonFromTuples :: (GL3Tuple, [GL3Tuple]) -- a 3-tuple for color and a list of 3-tuples for vertices
                  -> IO ()
polygonFromTuples (colorTuple, polygonTuples) = colorFromTuple colorTuple >> mapM_ verticesFromTuple polygonTuples

verticesFromTuple :: GL3Tuple -- a 3-tuple of GLfloats describing a Vertex3
                  -> IO ()
verticesFromTuple = vertex . (uncurry3 Vertex3)

colorFromTuple :: GL3Tuple -- a 3-tuple of GLfloats describing an RGB color
               -> IO ()
colorFromTuple = color . (uncurry3 Color3)


reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size) >> postRedisplay Nothing

uncurry3 :: (a -> b -> c -> d)      -- a function of three arguments
         -> (a, b, c)               -- a 3-tuple of those arguments
         -> d                       -- the function's result
uncurry3 f (a, b, c) = f a b c

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [(sin (2 * pi * k / 6), cos (2 * pi * k / 6), 0) | k <- [1..6]]
