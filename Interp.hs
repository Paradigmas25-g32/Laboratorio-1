module Interp
    (interp,
     interp_rotar,
     interp_espejar,
     interp_rotar45,
     interp_apilar,
     interp_juntar,
     interp_encimar,
     )
where

-- Sacar del esqueleto final!
module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar f = \v1 v2 v3 -> f (v1 V.+ v2) v3 (V.negate v2)

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar f = \v1 v2 v3 -> f (v1 V.+ v2) (V.negate v2) v3

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante

--interpreta el operador de apilar
interp_apilar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m f g = \v1 v2 v3 ->
    let
        t = n + m
        r = m `div` t 
        r' = n `div`t
        h' = r'*v3
        f' = f (v1 V.+ h') v2 (v3 V.* r)
        g' = g v1 v2 h'
    in
        f' V.+ g'

--interpreta el operador de juntar
interp_juntar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m f g = \v1 v2 v3 ->
    let
        t = n + m
        r = fromIntegral n / fromIntegral t
        r' = fromIntegral m / fromIntegral t -
        v2f = r V.* v2
        v2g = r' V.* v2
        gOffset = v1 V.+ v2f
        f' = f v1 v2f v3
        g' = g gOffset v2g v3
    in
        Pictures [f', g']



--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f = foldDib
    (\x -> \v1 v2 v3 -> f x v1 v2 v3)
    (\x -> interp_rotar (interp x))
    (\x -> interp_espejar (interp x))
    (\x -> interp_rotar45 (interp x))
    (\n m x y -> interp_apilar n m (interp x) (interp y))
    (\n m x y -> interp_juntar n m (interp x) (interp y))
    (\x y -> interp_encimar (interp x) (interp y))