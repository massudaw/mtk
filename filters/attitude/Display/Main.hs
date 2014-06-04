{-# LANGUAGE NoMonomorphismRestriction #-}
module Display.Main  where

import Control.Monad (liftM, unless, when)

import qualified Graphics.Rendering.GLU.Raw as GLU
import qualified Graphics.Rendering.OpenGL  as GL

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT

import qualified Display.Display as Display
import Control.Monad.IO.Class

import Pipes
import Pipes.Safe

import Linear.V3
import Linear.Matrix
import Rotation.SO3

configureDisplay :: IO ()
configureDisplay = do
    putStrLn "Initializing OpenGL"
    _ <- GLFW.initialize
    GLUT.initialize "" [""]
    _ <- GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_numRedBits   = 8
      , GLFW.displayOptions_numGreenBits = 8
      , GLFW.displayOptions_numBlueBits  = 8
      , GLFW.displayOptions_numDepthBits = 1
      }
    GLFW.setWindowSizeCallback windowSizeCallback
    GLFW.setWindowCloseCallback (return False)
    GL.clearColor    GL.$= GL.Color4 0.05 0.05 0.05 1
    GL.depthFunc     GL.$= Just GL.Less
    GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)
    GL.shadeModel    GL.$= GL.Smooth
    GL.lighting              GL.$= GL.Enabled
    GL.lightModelAmbient     GL.$= GL.Color4 0.2 0.2 0.2 1
    GL.position (GL.Light 0) GL.$= GL.Vertex4 (-10) 10 (-10) 0
    GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.4 0.4 0.4 1
    GL.diffuse  (GL.Light 0) GL.$= GL.Color4 0.8 0.8 0.8 1
    GL.light    (GL.Light 0) GL.$= GL.Enabled
    GL.lineSmooth GL.$= GL.Enabled
    GL.blend GL.$= GL.Enabled


windowSizeCallback :: Int -> Int -> IO ()
windowSizeCallback w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GLU.gluPerspective 45 (fromIntegral w / fromIntegral h) 0.1 100

v3toVec3 :: V3 Double -> GL.Vector3 GL.GLdouble
v3toVec3 (V3 x y z) = fmap realToFrac $ GL.Vector3 x y z


glSafe  = bracket
    (liftIO  configureDisplay)
    (liftIO . const stop )
    (start )

start () = loopPipe []
  where
    loopPipe xs = do
        SO3 angle <- await
        let xnew = v3toVec3 (angle !* (V3 1 0 0)) :xs
        q0 <- liftIO $ GLFW.keyIsPressed GLFW.KeyEsc
        q1 <- liftIO $ GLFW.keyIsPressed (GLFW.CharKey 'Q')
        if (q0 || q1)  
		then 
  		  liftIO stop
        	else  do
        	  liftIO $ loop 0 0 (xnew,angle)
        	  loopPipe $ take 50 xnew

    loop xa ya i = do
        GLFW.resetTime
        (jlr, jud) <- getJoystickDirections
        (klr, kud) <- getCursorKeyDirections
        let xa' = (xa +        jud * maxAngle) - kud
        let ya' = (ya + negate jlr * maxAngle) - klr
        t <- liftIO $ liftM (numSecondsBetweenFrames -) GLFW.getTime
        when (t > 0) (GLFW.sleep t)
        Display.draw xa' ya' i
      where
        maxAngle :: Float
        maxAngle = 1

        numSecondsBetweenFrames :: Double
        numSecondsBetweenFrames = recip (fromIntegral framesPerSecond)

        framesPerSecond :: Int
        framesPerSecond = 20

stop :: IO ()
stop = do
    putStrLn "Closing OpenGL"
    GLFW.closeWindow
    GLFW.terminate
    GLUT.exit

getJoystickDirections :: IO (Float, Float)
getJoystickDirections = do
    r <- take 2 `fmap` GLFW.getJoystickPosition GLFW.Joystick0 2
    return $
      case r of
        [x, y] -> (x, y)
        _      -> (0, 0)

getCursorKeyDirections :: IO (Float, Float)
getCursorKeyDirections = do
    l <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyLeft
    r <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyRight
    u <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyUp
    d <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyDown
    return (-l + r, -u + d)
  where
    toFloat b = if b then 1 else 0
