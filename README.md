# RecursividadHaskell


cociente :: Int -> Int-> Int
cociente n m = if n < m then 0
    else 1+cociente (n-m) m   
	
longitud :: Int->Int
longitud n = if n <= 10 then 1
    else 1+longitud (div n 10)
