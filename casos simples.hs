--Ejemplo bien simples de aplicación parcial
doble x = 2*x

doble' = (2 *)

{- consultas
*Main> map doble [1,2,3,4]
[2,4,6,8]
*Main> map doble' [1,2,3,4]
[2,4,6,8]
*Main> map (2*) [1,2,3,4]     
[2,4,6,8]
*Main> map (\x-> 2*x) [1,2,3,4]     
[2,4,6,8]
-}

func:: Int-> Int-> String -> Int -> Bool
func x y z w = x + y * length z > w

{-
*Main> func 2 3 "hola" 20
False
*Main> :t func
func :: Int -> Int -> String -> Int -> Bool
*Main> :t func 2
func 2 :: Int -> String -> Int -> Bool
*Main> (func 2) 3 "hola" 20
False
*Main> ((func 2) 3) "hola" 20
False
*Main> (((func 2) 3) "hola") 20
False
*Main> :t (func 2 3 "hola")   
(func 2 3 "hola") :: Int -> Bool
*Main> filter (func 2 3 "hola") [1..50]
[1,2,3,4,5,6,7,8,9,10,11,12,13]
-}
-- Otro ejemplo
promedio x y = (x + y)/2

sacarPromedio = promedio

promedioCon10 = promedio 10

{- Consultas
*Main> promedioCon10 8
9.0
*Main> promedio 10 8
9.0
-}

-- Otros ejemplos más

f:: Int->Bool
f 2 = True
f _ = False

g :: Int -> Int -> Bool
g 2 _ = True
g _ 0 = True
g _ _ = False


mejor   f   x    y 
--    (1+)  3  (-123)
   |  f x > f y = x
---  (1+) 3 > (1+) (-123) = 4 
---  1+3 > 1+ (-123) = 4
---  4 > (-122) = 4 

 
   | otherwise = y

