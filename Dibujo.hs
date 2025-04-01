module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a
              | Rotar (Dibujo a)
              | Espejo (Dibujo a)
              | Rotar45 (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving (Show, Eq, Ord)

-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a

r270 :: Dibujo a -> Dibujo a



-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a


-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a


-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a


-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a


-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a


-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib = Basica


-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib f _ _ _ _ _ _ (Basica a) = f a
foldDib f g h i j k l (Rotar d) = g (foldDib f g h i j k l d)
foldDib f g h i j k l (Espejo d) = h (foldDib f g h i j k l d)
foldDib f g h i j k l (Rotar45 d) = i (foldDib f g h i j k l d)
foldDib f g h i j k l (Apilar x y d1 d2) = j x y (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
foldDib f g h i j k l (Juntar x y d1 d2) = k x y (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
foldDib f g h i j k l (Encimar d1 d2) = l (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)


-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f = foldDib 
    (Basica . f) 
    Rotar 
    Espejo 
    Rotar45 
    Apilar 
    Juntar 
    Encimar 


-- Describe la figura. Ejemplos:
--   desc (const "b") (Basica b) = "b" --(un caso base).
--   desc db (Rotar fa) = "rot (" ++ desc db fa ++ ")" --(un caso recursivo).
-- La descripción de cada constructor son sus tres primeros
-- símbolos en minúscula.
desc :: (a -> String) -> Dibujo a -> String
desc f (Basica x) = f x
desc f (Rotar d) = "rot (" ++ desc f d ++ ")"
desc f (Espejo d) = "esp (" ++ desc f d ++ ")"
desc f (Rotar45 d) = "r45 (" ++ desc f d ++ ")"
desc f (Apilar x y d1 d2) = "api " ++ show x ++ " " ++ show y ++ " (" ++ desc f d1 ++ ") " ++ " (" ++ desc f d2 ++ ")"
desc f (Juntar x y d1 d2) = "api " ++ show x ++ " " ++ show y ++ " (" ++ desc f d1 ++ ") " ++ " (" ++ desc f d2 ++ ")"
desc f (Encimar d1 d2) = "enc (" ++ desc f d1 ++ ") " ++ " (" ++ desc f d2 ++ ")"


-- Junta todas las figuras básicas de un dibujo.
-- Devuelve una lista con todas las figuras básicas que componen un dibujo.
basicas :: Dibujo a -> [a]
basicas = foldDib
       (\x ->[x]) -- Basica
       id -- Rotar
       id -- Espejo
       id -- Rotar45
       (\_ _ d1 d2 -> d1 ++ d2) -- Apilar
       (\_ _ d1 d2 -> d1 ++ d2) -- Juntar
       (\d1 d2 -> d1 ++ d2) -- Encimar
