cuadrado :: Float -> Float
cuadrado n = n * n

invertir :: Float -> Float
invertir n = 1 / n

sumatorio1pn :: Int -> Float
sumatorio1pn 0 = 0
sumatorio1pn n = (invertir (cuadrado n )) + (sumatorio1pn (n - 1))

aproxPi :: Integer -> Float
aproxPi n = sqrt ((sumatorio1pn n ) * 6)
