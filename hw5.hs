cyrcShiftL :: Int -> [a] -> [a]
cyrcShiftL n [] = []
cyrcShiftL 0 x = x
cyrcShiftL n xs | n > 0 = cyrcShiftL (n-1) (tail xs)++[head xs]
	        | n < 0 = cyrcShiftL (n+1) ((last xs):(init xs))


