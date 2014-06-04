{-# LANGUAGE TypeSynonymInstances ,NoMonomorphismRestriction,FlexibleInstances#-}

module Display.Cube
  ( draw
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

import Data.StateVar
import Linear.Matrix
import Linear.V3
import Linear.V4
import Control.Lens

import Data.Functor.Compose
--import Vectorization
import Data.Foldable(toList)
import Foreign hiding(rotate)

import Rotation.SO3

instance GL.Matrix (Compose V4 V4) where
    withMatrix m f =withArray (toList m) (f GL.RowMajor)




drawBall c (x,l)  = do
    GL.preservingMatrix $ do
        GL.translate l
        GL.color $ c (x::GL.GLfloat)
        GLUT.renderObject GLUT.Solid (GLUT.Sphere' (0.1*(realToFrac x)) 10 10)

vec3ToVertex3 (GL.Vector3 x y z) = GL.Vertex3 x y z

vertex3ToVector3 (GL.Vertex3 x y z) = GL.Vector3 x y z

data Box a
    = Box
    { position :: V3 a
    , size :: V3 a
    , orientation :: SO3 a
    }deriving(Show)


v3ToVector3 (V3 x y z ) = fmap realToFrac (Vector3 x y z)

transformation (SO3 orient)=  m43_to_m44 $ vector orient

drawBox (Box pos len orient) = do
    multMatrix (fmap realToFrac $ Compose $ transformation orient :: Compose V4 V4 GLdouble)
    translate  (v3ToVector3 pos :: Vector3 GLdouble)
    let (V3 l w h)  = fmap realToFrac len :: V3 GLdouble
    GL.scale  l w h
    GLUT.renderObject GLUT.Wireframe (GLUT.Cube 1)


drawFloor s = do
    scale s s s
    rotate (-90) (Vector3 0 1 0 :: Vector3 GLdouble)
    rotate (-90) (Vector3 1 0 0 :: Vector3 GLdouble)
    rotate 0 (Vector3 0 0 1 :: Vector3 GLdouble)
    rotate 0 (Vector3 0 1 0 :: Vector3 GLdouble)
    scale 1 1 (0.1 :: GLdouble)
    GLUT.renderObject GLUT.Solid (GLUT.Cube 1)
    scale 1 1 (10 :: GLdouble )
    scale s s s



drawAxisElem l a c =   GL.preservingMatrix $ do
        color c 
	GL.renderPrimitive Lines $ do
          vertex (Vertex3 0 0 0 :: Vertex3 GLdouble) 
          vertex a
        translate $ vertex3ToVector3 a
        GLUT.renderObject GLUT.Solid (GLUT.Sphere'  0.02 10 10)
        let s = 0.0010 :: GLfloat
            fscale = scale s s s
        fscale
        color white
        GLUT.renderString GLUT.Roman  l
 
drawAxis lz ly lx= do
    let origin ,ax,ay,az:: Vertex3 GLfloat
        origin = Vertex3 0 0 0
        ax = Vertex3 0.25 0 0
        ay = Vertex3 0 0.25 0
        az = Vertex3 0 0 0.25
    drawAxisElem lx ax blue
    drawAxisElem ly ay green 
    drawAxisElem lz az red 

showVec3 (Vector3 x y z) = "(" ++ (show x ) ++ " , " ++ (show y ) ++ " , " ++ (show z ) ++")"


draw s (l,i) = do
    GL.color white
    GL.preservingMatrix $ do
        translate $ Vector3 (-0.9::GLfloat) (-0.9) 0
        scale 0.0002 0.0002 (0.0002 ::GLfloat)
        color white
        GLUT.renderString  GLUT.Roman (show $ angles (SO3 i))
    GL.color gray
    drawFloor s
    translate (Vector3 0 0 1 ::Vector3 GLdouble)
    drawAxis "North" "East" "Up"
    GL.preservingMatrix $ do
        GL.color green
        renderPrimitive LineStrip $
            mapM_ (vertex . vec3ToVertex3)l
    let ld = fromIntegral (length l)
    GL.preservingMatrix $ do
        GL.color blue
        mapM_ (drawBall blueg ) $ zip (map (^2) $ map (/ld) [ld,(ld-1)..1]) l
    GL.preservingMatrix $ do
        GL.color red
        drawBox (Box 0  (V3 1 1 1) (SO3 i))
        drawAxis "Ax" "Ay" "Az"
    GL.preservingMatrix $ do
        renderPrimitive Lines $ do
            vertex (vec3ToVertex3 $ head l)
            vertex (Vertex3 0 0 (0::GLfloat) )
        GL.translate (head l)
        GL.color red
        GLUT.renderObject GLUT.Solid (GLUT.Sphere'  0.1 10 10)

white,red, green,gray, blue :: GL.Color4 GL.GLfloat
red   = GL.Color4 1 0 0 1
green = GL.Color4 0 1 0 1
blue  = GL.Color4 0 0 1 1
blueg x = GL.Color4 0 0 1 x
white = GL.Color4 (1/3) (1/3) (1/3) 1
gray = GL.Color4 (1) (1) (1) 1

z, n1, p1 :: GL.GLfloat
z  =  0
n1 = -1
p1 =  1
