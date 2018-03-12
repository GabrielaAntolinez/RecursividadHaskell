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
	
--Sumar Pares Funciones
sumapa::[Int]->Int
sumapa []=0
sumapa lista= foldl(+)0[x | x <- lista, mod x 2 ==0]

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
	
--Contar impares funciones

cantimpar::[Int]->Int
cantimpar []=0
cantimpar (x:xs)= length [x | x <- (x:xs), mod x 2 ==1]

--Contar impares recursivo
impares::[Int]->Int
impares [] = 0
impares (x:xs)
	|not ((mod x 2) ==  0) = impares (xs) +1
	|otherwise = impares (xs) 

-- contiene recursividad
enLista::Int->[Int]->Bool
enLista x []= False
enLista x (y:ys)= x==y || enLista x (ys)

contiene:: [Int]->[Int]->Bool
contiene [] []=True
contiene [] _=False
contiene _ []=True
contiene (x:xs) (y:ys) = enLista y (x:xs) && contiene (x:xs) ys 

--contiene funciones
contiene:: [Int]->[Int]->Bool
contiene (x:xs) (y:ys) = foldl (&&) True [enLista s (x:xs) | s<- (y:ys)]

	
--Mayor de Lista

mayor::[Int]->Int
mayor [ ] = 0
mayor (x:xs)
  |x>mayor(xs)=x
  |otherwise=mayor(xs)
  

    
