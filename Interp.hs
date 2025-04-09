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
interp_rotar45 f v1 v2 v3 = f (v1 V.+ mitad (v2 V.+ v3)) (mitad (v2 V.+ v3)) (mitad (v3 V.- v2))

--interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m f g = \v1 v2 v3 ->
    let
        t = n + m
        r = m / t 
        r' = n / t
        h' = r' V.* v3
        f' = f (v1 V.+ h') v2 (r V.* v3)
        g' = g v1 v2 h'
    in
        Pictures [f', g']--interpreta el operador de juntar


--interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m f g = \v1 v2 v3 ->
    let
        t = n + m
        r = n / t
        r' = m / t 
        v2f = r V.* v2
        v2g = r' V.* v2
        gOffset = v1 V.+ v2f
        f' = f v1 v2f v3
        g' = g gOffset v2g v3
    in
        Pictures [f', g']

--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar f g = \v1 v2 v3 -> Pictures [f v1 v2 v3, g v1 v2 v3]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f = foldDib f interp_rotar interp_espejar interp_rotar45 interp_apilar interp_juntar interp_encimar

