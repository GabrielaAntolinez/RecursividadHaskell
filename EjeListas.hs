module EjerListas where
--Invertir lista
invert::[Int]->[Int]
invert [ ] = [ ]
invert (x:xs) = (invert xs)++[x]

--Sumar Pares

sumarPares::[Int]->Int
sumarPares [] = 0
sumarPares (x:xs)
	|(mod x 2) == 0 = pares (xs) +x
	|otherwise = pares (xs) 
  
-- Contar pares

pares::[Int]->Int
pares [] = 0
pares (x:xs)
	|(mod x 2) == 1 = pares (xs) +1 
	|otherwise = pares (xs) 
-- comparar listas

comparar::[Int]->[Int]->Bool
comparar [][]= True
comparar (x:xs)[]=False
comparar [](x:xs)= False
comparar (x:xs)(y:ys)
	| (x)==(y) = comparar(xs)(ys) && True
	| not((x)==(y))= comparar(xs)(ys) && False
	|otherwise= comparar(xs)(ys)
	
--Contar impares 

cantimpar::[Int]->Int
cantimpar []=0
cantimpar (x:xs)= length [x | x <- (x:xs), mod x 2 ==1]

impares::[Int]->Int
impares [] = 0
impares (x:xs)
	|not ((mod x 2) ==  0) = impares (xs) +1
	|otherwise = impares (xs) 
	
--Mayor de Lista

mayor::[Int]->Int
mayor [ ] = 0
mayor (x:xs)
  |x>mayor(xs)=x
  |otherwise=mayor(xs)
  

    
