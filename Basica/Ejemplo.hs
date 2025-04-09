module Basica.Ejemplo where
import Dibujo
import Interp
import Basica.Comun

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = espejar(basica ())

interpBas :: Basica -> ImagenFlotante
interpBas () = formaF
