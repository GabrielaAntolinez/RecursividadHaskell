module EjerListas where
--Invertir lista
invert::[Int]->[Int]
invert [ ] = [ ]
invert (x:xs) = (invert xs)++[x]

--Mayor de Lista
mayor::[Int]->Int
mayor [ ] = 0
mayor (x:xs)
  |x>mayor(xs)=x
  |otherwise=mayor(xs)
    
