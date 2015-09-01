import Graphics.UI.GLUT
import Data.IORef

type GL3Tuple = (GLfloat, GLfloat, GLfloat)

main :: IO ()
main = do
  (_, _) <- getArgsAndInitialize
  _window <- createWindow "hexagons are pretty cool"
  angle <- newIORef 0.0
  displayCallback $= display angle
  idleCallback $= Just (idle angle)
  mainLoop

display :: IORef GLfloat -> DisplayCallback
display angle = do
    clear [ColorBuffer]
    a <- get angle
    drawPoints a
    flush
  where
    drawPoints :: GLfloat -> IO ()
    drawPoints rotationAngle = do
        mapM_ (renderPrimitive Polygon) (map polygonFromTuples (polygons rotationAngle))
      where
        polygons :: GLfloat -> [(GL3Tuple, [GL3Tuple])]
        polygons rotateAngle = let phi k = 2 * pi * (k + rotateAngle) / 6
          in
          [ ((1, 0, 0), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [1..2]]),
          ((0.9, 0, 0.3), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [2..3]]),
          ((0.8, 0, 0.4), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [3..4]]),
          ((0.7, 0, 0.5), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [4..5]]),
          ((0.8, 0, 0.4), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [5..6]]),
          ((0.9, 0, 0.3), [(0, 0, 0)] ++ [(sin (phi k), cos (phi k), 0) | k <- [6..7]]) ]

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 0.001)
  postRedisplay Nothing

polygonFromTuples :: (GL3Tuple, [GL3Tuple]) -> IO ()
polygonFromTuples (colorTuple, polygonTuples) = colorFromTuple colorTuple >> mapM_ verticesFromTuple polygonTuples

verticesFromTuple :: GL3Tuple -> IO ()
verticesFromTuple = vertex . (uncurry3 Vertex3)

colorFromTuple :: GL3Tuple -> IO ()
colorFromTuple = color . (uncurry3 Color3)


reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size) >> postRedisplay Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [(sin (2 * pi * k / 6), cos (2 * pi * k / 6), 0) | k <- [1..6]]
