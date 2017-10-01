err :: Double
err = 10 ** (-6)

partsum :: Double -> Int -> Double -> Double -> Double
partsum x i res eps = 
	let fact x = if x <= 1 then 1 else x * fact (x - 1)  in
	let pow i = 2 * i - 1 in
	if abs x ** (pow (fromIntegral i)) / (fact (pow (fromIntegral i))) < eps 
	then res
	else if i `mod` 2 == 0 
		then partsum x (i + 1) (res - abs x ** (pow (fromIntegral i)) / (fact (pow (fromIntegral i)))) eps
		else partsum x (i + 1) (res + abs x ** (pow (fromIntegral i)) / (fact (pow (fromIntegral i)))) eps

sin_ :: Double -> Double
sin_ x =
	partsum x 1 0 err