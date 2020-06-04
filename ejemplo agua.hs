--Aplicación parcial

data Barrio = UnBarrio {
    poblacion:: Integer,
    nombre :: String,
    conAgua:: Float
} deriving Show

al, pa, b31, once::Barrio
al = UnBarrio 200 "almagro" 0.95
pa = UnBarrio 500 "palermo" 0.99
b31 = UnBarrio 100 "barrio 31" 0.5
once = UnBarrio 300 "balvanera" 0.95

cantidadPersonasSinAgua :: Barrio -> Integer
cantidadPersonasSinAgua barrio = poblacion barrio - ceiling (fromIntegral (poblacion barrio) * conAgua barrio)

ampliarRed:: Float -> Barrio -> Barrio
ampliarRed nro barrio = barrio{conAgua = conAgua barrio + nro}

renombrar:: String -> Barrio -> Barrio
renombrar nuevoNombre barrio = barrio{nombre = nuevoNombre}

registroCivil:: Integer -> Integer -> Barrio -> Barrio
registroCivil nacimientos defunciones barrio = barrio{poblacion = poblacion barrio + nacimientos - defunciones}

duplicar::Barrio->Barrio
duplicar barrio = barrio{poblacion = 2*poblacion barrio}

fusionar:: Barrio -> Barrio -> Barrio
fusionar unBarrio otroBarrio = UnBarrio { 
    poblacion = poblacion unBarrio + poblacion otroBarrio,
    nombre = take 4 (nombre unBarrio) ++ drop 4 (nombre otroBarrio),
    conAgua = promedio (conAgua unBarrio) (conAgua otroBarrio)    }
{- 
*Main> fusionar al pa
UnBarrio {poblacion = 700, nombre = "almarmo", conAgua = 0.97}
*Main> fusionar pa al
UnBarrio {poblacion = 700, nombre = "palegro", conAgua = 0.97}
-}

reformulacion::[Barrio]->[Barrio]
reformulacion barrios = map (ampliarRed 0.01) barrios
--reformulacion barrios = map (\ b -> ampliarRed 0.01 b) barrios

{-
*Main> reformulacion [al,pa,b31]
[UnBarrio {poblacion = 200, nombre = "almagro", conAgua = 0.96},UnBarrio {poblacion = 500, nombre = "palermo", conAgua = 1.0},UnBarrio {poblacion = 100, nombre = "barrio 31", conAgua = 0.51}]
-}

dictadura::[Barrio]->[Barrio]
dictadura barrios = map (renombrar "dictador") barrios

reformaLoca::Barrio -> [Barrio]->[Barrio]
reformaLoca barrio barrios = map (fusionar barrio) barrios

listaDeModificaciones::[Barrio->Barrio]
listaDeModificaciones = [ampliarRed 0.1, renombrar "nuevo", registroCivil 10 2, duplicar]


composicion :: (Int->Bool) -> (Int->Int)-> Int -> Bool
composicion f g x = f (g x)
--Ojo! este tipo está restringido a propósito para el ejemplo. 
--La funcion composicion de haskell (.) es mas genérica
--(.) f g x = f (g x)

{-
*Main> composicion even doble 3
True
*Main> composicion even (2*) 3 
True
*Main> composicion (4<=) (2*) 3  
True
*Main> filter ((4<=).doble)  [3,2,5,6]
[3,5,6]
*Main> filter (\x -> 4 <= doble x)  [3,2,5,6]
[3,5,6]
*Main> filter ((>4).doble)  [3,2,5,6]
[3,5,6]
-}
