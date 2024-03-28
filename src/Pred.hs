module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where
import Dibujo (foldDib, Dibujo, change, figura)
import GHC.GHCi.Helpers (disableBuffering)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f  = change fun
                  where fun n = if p n then f n else figura n 

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib p id id id ordFold ordFold (||)
              where ordFold _ _  = (||)  

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib p id id id andFold andFold (&&)
              where andFold _ _ = (&&)

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 a = (p1 a) && (p2 a) 

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 a = (p1 a) || (p2 a)

falla = True


