module Interp
  ( interp,
    initial,
  )
where

import Dibujo
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Data.Vector (mulSV)


-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = pictures [p,q] 

r45 :: FloatingPic -> FloatingPic
r45 pict d w h = pict (d V.+ a) a b
                where 
                    a = mulSV 0.5 (w V.+ h)
                    b = mulSV 0.5 (h V.- w)

rot :: FloatingPic -> FloatingPic
rot pict d w h = pict (d V.+ w) h (V.negate w)

esp :: FloatingPic -> FloatingPic
esp pict d w h = pict (d V.+ w) (V.negate w) h

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup p1 p2 d w h = ov (p1 d w h) (p2 d w h)

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n pic1 pic2 d w h = ov (pic1 d w' h) (pic2 (d V.+ w') w'' h)
                          where
                              r' = n/(n+m)
                              r  = m/(n+m)
                              w' = mulSV r w
                              w''= mulSV r' w    


api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api m n pic1 pic2 d w h = ov (pic1 (d V.+ h') w h'') (pic2 d w h') 
                          where 
                            r'  = n/(n+m)
                            r   = m/(n+m)
                            h'  = mulSV r' h
                            h'' = mulSV r  h


interp :: Output a -> Output (Dibujo a)
interp b = foldDib b rot esp r45 api jun sup