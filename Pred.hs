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
esRot360 dib = (foldDib (\x -> 0)                                   --basica
                        (\x -> if x `div` 4 >= 1              --chequeo si hubo 4 rot
                                    then x                          --hay 4 rot
                                    else 1 + x )                    --no hay 4 rot -> sigo contando

                                                                    --espejar
                        (\x -> if x `div` 4 >= 1                    --hubo 4 rotar?
                                then x                              --hay 4 rotar
                                else 0)                             --cuento de nuevo
                                                                    
                                                                    --rot45
                        (\x -> if x `div` 4 >= 1                    --hubo 4 rotar?
                                then x                              --hay 4 rotar
                                else 0)                             --cuento de nuevo                                      
                                                                    --apilar
                        (\_ _ d1 d2 -> if d1 `div` 4 >= 1          --chequeo si dib1 tiene 4 rot
                                        then 4 
                                        else if d2 `div` 4 >= 1     --chequeo si dib2 tiene 4 rot
                                            then 4
                                            else 0)                     
                                                                    --juntar
                        (\_ _ d1 d2 -> if d1 `div` 4 >= 1           --chequeo si dib1 tiene 4 rot
                                        then 4 
                                        else if d2 `div` 4 >= 1     --chequeo si dib2 tiene 4 rot
                                            then 4
                                            else 0)                     
                                                                    --encimar
                        (\d1 d2 -> if d1 `div` 4 >= 1               --chequeo si dib1 tiene 4 rot
                                    then 4                          
                                    else if d2 `div` 4 >= 1         --chequeo si dib2 tiene 4 rot
                                        then 4
                                        else 0) 
                        dib) == 4       


-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 dib = (foldDib (\x -> 0)                                    --basica
                                                                    --rotar
                       (\x -> if x `div` 2 >= 1                     --hubo 2 espejar?
                                then x                              -- hay 2 espejar
                                else 0)                             -- cuento de nuevo

                                                                    --espejar
                       (\x -> if x `div` 2 >= 1               --chequeo que hubo 2 espejar
                                    then x                          -- si hay
                                    else 1 + x  )                   -- sigo contando
                        
                       (\x -> if x `div` 2 >= 1                     --rot45, similar a rotar
                                then x 
                                else 0) 
                        
                       (\_ _ d1 d2 -> if d1 `div` 2 >= 1            --apilar
                                        then 2 
                                        else if d2 `div` 2 >= 1
                                            then 2
                                            else 0) 
                       (\_ _ d1 d2 -> if d1 `div` 2 >= 1            --juntar
                                        then 2 
                                        else if d2 `div` 2 >= 1 
                                            then 2
                                            else 0) 
                       (\d1 d2 -> if d1 `div` 2 >= 1                --encimar
                                        then 2 
                                        else if d2 `div` 2 >= 1 
                                            then 2
                                            else 0)
                         dib) == 2

data Superfluo = RotacionSuperflua | FlipSuperfluo

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)


