{-
modulo ejercicios recurividad
-}

module EjerciciosRecursividad where 

-- calcula el producto mediante sumas

producto :: Int->Int->Int 
producto n 0 = 0
producto n 1 = n
producto n m = n +producto n (m-1)

-- cociente de un numero mediante restas

cociente :: Int -> Int-> Int 
cociente n m = if n < m then 0 
		else 1+ cociente (n-m) m


--  calcula la potencia mediante multiplicaciones

potencia :: Int->Int->Int
potencia n 0 = 1
potencia n 1 = n
potencia n m = n*potencia n (m-1)


-- mayor digito de un numero 
mayorDig :: Int->Int 
mayorDig n 
	| n<10 = n 
	| otherwise = if (mayorDig(mod n 10)*10>mayorDig(div n 10)*10) then mayorDig(mod n 10) 
				else mayorDig(div n 10)
				
-- mirar si un numero es palindromo 

palindromo :: Int->Bool
palindromo n 
	| n<10 = True 
	| otherwise = if n == invertir n then True
					else False

-- calcula la serie de fibonacci del numero 

fibonacci :: Int->Int 
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- suma de los digitos de un numero digitos

sumaDig :: Int->Int
sumaDig n 
	| n<10 = n
	| otherwise = sumaDig (div n 10) + mod n 10
	
	
-- hallar la longitud de un numero

longitud :: Int->Int 
longitud n = if n <= 10 then 1 
		else 1+longitud (div n 10)

-- invertir un numero 

invertir :: Int->Int
invertir n 
	| n<10 = n
	| otherwise = (mod n 10)* (potencia 10 ((longitud n)- 1)) + invertir (div n 10) 
			