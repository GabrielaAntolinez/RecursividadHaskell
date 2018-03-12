module EjerListas where
--Invertir lista
invert::[Int]->[Int]
invert [ ] = [ ]
invert (x:xs) = (invert xs)++[x]

--Sumar Pares
sumarPares::[Int]->Int
sumarPares [] = 0
sumarPares (x:xs)
	|(mod x 2) == 0 = sumarPares (xs) +x
	|otherwise = sumarPares (xs) 
  
-- contar pares

pares::[Int]->Int
pares [] = 0
pares (x:xs)
	|(mod x 2) == 1 = pares (xs) +1 
	|otherwise = pares (xs) 
  
--Contar impares 
cantimpar::[Int]->Int
cantimpar []=0
cantimpar (x:xs)= length [x | x <- (x:xs), mod x 2 ==1]
  
--Mayor de Lista
mayor::[Int]->Int
mayor [ ] = 0
mayor (x:xs)
  |x>mayor(xs)=x
  |otherwise=mayor(xs)
    
