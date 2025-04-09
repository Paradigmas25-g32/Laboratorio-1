module Pred where

import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f dib= foldDib (\a -> if p a then f a else basica a)
                         rotar espejar rotar45 apilar juntar encimar dib

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred dib = foldDib pred id id id                 --rotar espejar rot45 
                       (\_ _ d1 d2 -> d1 || d2)         --apilar
                       (\_ _ d1 d2 -> d1 || d2)         --juntar
                       (\d1 d2 -> d1 || d2) dib         --encimar
                       
-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred dib = foldDib pred id id id
                       (\_ _ d1 d2 -> d1 && d2)
                       (\_ _ d1 d2 -> d1 && d2)
                       (\d1 d2 -> d1 && d2) dib

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)

data Superfluo = RotacionSuperflua | FlipSuperfluo

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)


