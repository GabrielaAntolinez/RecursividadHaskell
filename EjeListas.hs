module EjerListas where
--Invertir lista
invert::[Int]->[Int]
invert [ ] = [ ]
invert (x:xs) = (invert xs)++[x]

--Cantidad de impares
cantimpar::[Int]->Int
cantimpar []=0
cantimpar (x:xs)= length [x | x <- (x:xs), mod x 2 ==1]

--Mayor de Lista
mayor::[Int]->Int
mayor [ ] = 0
mayor (x:xs)
  |x>mayor(xs)=x
  |otherwise=mayor(xs)
    
