module Basics.Ejemplo where
import Dibujo
import Interp
import Basics.Comun

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = espejar(basica ())

interpBas :: Basica -> ImagenFlotante
interpBas () = formaF
