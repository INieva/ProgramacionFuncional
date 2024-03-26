module Dibujo (Dibujo, figura, rotar, espejar, 
              rot45, juntar, encimar, r180, r270,r90,
              (///), (^^^), (.-.), encimar4, ciclar,
              cuarteto, mapDib, foldDib,change
    -- agregar las funciones constructoras
) where


-- nuestro lenguaje 
data Dibujo a = Basica a|Rotar (Dibujo a) |Espejar (Dibujo a)|
                Rot45 (Dibujo a)|Apilar Float Float (Dibujo a) (Dibujo a)|
                Juntar Float Float (Dibujo a) (Dibujo a)|
                Encimar (Dibujo a) (Dibujo a) deriving(Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 f x = f x
comp n f x = f (comp (n-1) f x)


-- Funciones constructoras
figura :: a -> Dibujo a
figura = Basica

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar


espejar :: Dibujo a -> Dibujo a
espejar = Espejar

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^)  = encimar 

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.)  = apilar  1 1  

(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///)  = juntar 1 1 

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = rotar

r180 :: Dibujo a -> Dibujo a
r180 = comp 1 rotar 

r270 :: Dibujo a -> Dibujo a
r270 = comp 2 rotar

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (^^^) ((^^^) d (r90 d)) ((^^^) (r180 d) (r270 d))

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (.-.) ((///) d1 d2) ((///) d3 d4)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d d1 d2 d3 
          where d1 = rotar d
                d2 = r180 d
                d3 = r270 d

-- map para nuestro lenguaje
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a)         = Basica (f a)
mapDib f (Rotar d)          = Rotar (mapDib f d) 
mapDib f (Espejar d)        = Espejar (mapDib f d) 
mapDib f (Rot45 d)          = Rot45 (mapDib f d) 
mapDib f (Apilar n m d1 d2) = Apilar n m (mapDib f d1) (mapDib f d2)
mapDib f (Juntar n m d1 d2) = Juntar n m (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2)    = Encimar (mapDib f d1) (mapDib f d2) 


-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Basica a)         = f a 
change f (Rotar d)          = Rotar (change f d) 
change f (Espejar d)        = Espejar (change f d)
change f (Rot45 d)          = Rot45 (change f d)
change f (Apilar n m d1 d2) = Apilar n m (change f d1) (change f d2)
change f (Juntar n m d1 d2) = Juntar n m (change f d1) (change f d2)
change f (Encimar d1 d2)    = Encimar (change f d1) (change f d2)

-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib bas _ _ _ _ _ _ (Basica a) =  bas a
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Rotar dib)  = rotar' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Espejar dib) = espejar' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Rot45 dib) = rot45' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Apilar n m  d1 d2) = apilar' n m (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' d1) (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' d2)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Juntar n m  d1 d2) = juntar' n m (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' d1) (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' d2)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Encimar d1 d2) = encimar' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' d1) (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' d2)