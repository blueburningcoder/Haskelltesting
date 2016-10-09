module Cube where

import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x,y,z) = vertex $ Vertex3 x y z


cubeFrame :: GLfloat -> IO ()
cubeFrame w = do
  renderPrimitive Lines $ mapM_ vertex3f
    [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
      (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
      ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
      (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
      ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
      (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]


cube :: GLfloat -> IO ()
cube w = do
  renderPrimitive Quads $ mapM_ vertex3f 
    [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
      ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
      ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
      (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
      ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
      ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

quads :: IO ()
quads = do
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Inside FillStyle) $ Disk 0.0 (0.5 :: GLdouble) 1 2
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Inside PointStyle) $ Disk 0.5 (0.1 :: GLdouble) 2 3
  renderQuadric (QuadricStyle (Just Smooth) NoTextureCoordinates Outside FillStyle) $ Disk 0.0 (0.8 :: GLdouble) 1 10
  renderQuadric (QuadricStyle (Just Flat) GenerateTextureCoordinates Inside FillStyle) $ Disk 0.0 (0.5 :: GLdouble) 10 1



