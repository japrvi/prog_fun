{-Ejercicio 0
	- -}
cambiarvalor :: Eq a => a -> b -> [(a , b)] -> [(a , b)]
cambiarvalor e v [] = []
cambiarvalor e v [x:resto] = if fst x = e 
                             then [(e , v)] + cambiarvalor e v [resto]
							 else [x] + cambiarvalor e v [resto]

{-Ejercicio 1
	- -}
monticulos :: Integer -> [[Integer]]
monticulos 0 = [[0]]
monticulos n = monticulos (n - 1) + [[0:n]]

{-Ejercicio 3-}
contar [a] -> a -> Int
contar [] r = 0
contar [x:resto] r = if x == r
                     then 1 + contar resto
					 else 0 + contar resto

ganador :: [a] -> a -> a -> a
ganador l r s
	| a < b = r
	| a > b = s
	| a == b = "empate"
	where a = contar l r
		  b = contar l s
{-Ejercicio 4 
-Para evitar conversiones de tipos se ha asumido que
- el valor suministrado por parametro es un flotante
- que solo admite valores enteros-}
cuadrado :: Float -> Float
cuadrado n = n * n

invertir :: Float -> Float
invertir n = 1 / n

sumatorio1pn :: Float -> Float
sumatorio1pn 0 = 0
sumatorio1pn n = (invertir (cuadrado n )) + (sumatorio1pn (n - 1))

aproxPi :: Float -> Float
aproxPi n = sqrt ((sumatorio1pn n ) * 6)
