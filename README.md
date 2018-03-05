# RecursividadHaskell

mayorDig :: Int->Int
mayorDig n
	| n<10 = n
    	| otherwise = if (mayorDig(mod n 10)*10>mayorDig(div n 10)*10) then mayorDig(mod n 10)
					else mayorDig(div n 10)
