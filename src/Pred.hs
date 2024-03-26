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
cambiar pred fun  = change pred'
                  where pred' n = if pred n then fun n else figura n 

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred = foldDib pred id id id ordFold ordFold (||)
              where ordFold _ _  = (||)  

-- Todas las básicas satisfacen el predicado.
allDib = undefined

-- Los dos predicados se cumplen para el elemento recibido.
andP = undefined

-- Algún predicado se cumple para el elemento recibido.
orP = undefined

falla = True


