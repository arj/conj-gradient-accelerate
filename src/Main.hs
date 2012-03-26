{-# LANGUAGE CPP, BangPatterns #-}

module Main where

import Cloth
import Config
import Data.IORef
import System.Environment
import Data.Array.Accelerate      ( Array, Scalar, Exp, Acc,DIM1,DIM0,DIM2, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate as A
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as CUDA
#endif
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad

points :: [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
points = map f triangles
  where
    triangles =  [(0,1,4),(1,5,4),(1,5,2),(2,6,5),(2,3,6),(3,7,6),(4,5,8),(5,9,8),(5,6,9),(6,10,9),(6,7,10),(7,11,10),(8,9,12),(9,13,12),(9,10,13),(10,14,13),(10,11,14),(11,15,14)]
    points = map (\(x,y,z) -> (x*0.1,y*0.1,z*0.1)) [(0.0,0.0,0.0),(1.0,0.0,0.0),(2.0,0.0,0.0),(3.0,0.0,0.0),(0.0,0.0,1.0),(1.0,0.0,1.0),(2.0,0.0,1.0),(3.0,0.0,1.0),(0.0,0.0,2.0),(1.0,0.0,2.0),(2.0,0.0,2.0),(3.0,0.0,2.0),
              (0.0,0.0,3.0),(1.0,0.0,3.0),(2.0,0.0,3.0),(3.0,0.0,3.0)]
    f (i,j,k) = let (xi,yi,zi) = points !! i in
                let (xj,yj,zj) = points !! j in
                let (xk,yk,zk) = points !! k in
                ((xi,yi,zi),(xj,yj,zj),(xk,yk,zk))
         

main :: IO ()
main = do
  (config, nops) <- processArgs =<< getArgs
  (progname, _) <- getArgsAndInitialize
  initialWindowSize $= Size 800 800
  createWindow "Cloth simulation"

  shadeModel $= Smooth
  clearColor $= Color4 (0.3 :: GLclampf) 0.3 0.3 0

  pos   <- newIORef ((0,0,-4) :: (GLfloat,GLfloat,GLfloat))
  rot   <- newIORef ((30,30,0) :: (GLfloat,GLfloat,GLfloat))
  state <- newIORef ((0) :: GLint)

  keyboardMouseCallback $= Just (keyboardMouse pos rot state)
  idleCallback $= Just (idle rot)
  displayCallback $= (display pos rot state )
  reshapeCallback $= Just reshape
  mainLoop

drawWireframe points = do
   mapM_ (\((xi,yi,zi),(xj,yj,zj),(xk,yk,zk)) -> 
      renderPrimitive LineLoop $ do
         color  $ (Color3 (0.1::GLfloat) 0.1 0.1)
         vertex $ Vertex3 xk yk zk
         vertex $ Vertex3 xj yj zj
         vertex $ Vertex3 xi yi zi
         ) points

display pos rot state = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  (x',y',z') <- get pos
  (xr,yr,zr) <- get rot
  (grid) <- get state
  translate $ Vector3 x' y' z'
  rotate xr $ Vector3 (1 :: GLfloat) 0 0
  rotate yr $ Vector3 (0 :: GLfloat) 1 0
  rotate zr $ Vector3 (0 :: GLfloat) 0 1
  renderPrimitive Triangles $ (color $ (Color3 (0::GLfloat) 0.7 1)) >> mapM_ (\((xi,yi,zi),(xj,yj,zj),(xk,yk,zk)) -> (vertex$Vertex3 xk yk zk) >> (vertex$Vertex3 xj yj zj) >> (vertex$Vertex3 xi yi zi)) points
  when (grid == 1) $ drawWireframe points
  swapBuffers

idle rot = do
  postRedisplay Nothing

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral w/fromIntegral h) 0.1 100
  matrixMode $= Modelview 0
  loadIdentity
  postRedisplay Nothing


keyboardMouse pos rot mystate (Char 'a') state modifiers position = do
  (x',y',z') <- get pos
  pos $= (x' + 0.05, y', z')

keyboardMouse pos rot mystate (Char 'd') state modifiers position = do
  (x',y',z') <- get pos
  pos $= (x' - 0.05, y', z')

keyboardMouse pos rot mystate (Char 'w') state modifiers position = do
  (x',y',z') <- get pos
  pos $= (x', y', z' + 0.1)

keyboardMouse pos rot mystate (Char 's') state modifiers position = do
  (x',y',z') <- get pos
  pos $= (x', y', z' - 0.1)

keyboardMouse pos rot mystate (Char 'q') state modifiers position = do
  (x',y',z') <- get pos
  pos $= (x', y' - 0.1, z')

keyboardMouse pos rot mystate (Char 'z') state modifiers position = do
  (x',y',z') <- get pos
  pos $= (x', y' + 0.1, z')

keyboardMouse pos rot mystate (Char '1') state modifiers position = mystate $= (1)
keyboardMouse pos rot mystate (Char '2') state modifiers position = mystate $= (0)

keyboardMouse pos rot mystate (SpecialKey KeyRight) state modifiers position = do
  (x',y',z') <- get rot
  rot $= (x', y' - 1, z')

keyboardMouse pos rot mystate (SpecialKey KeyLeft) state modifiers position = do
  (x',y',z') <- get rot
  rot $= (x', y' + 1, z')

keyboardMouse pos rot mystate key state modifiers position = return ()
