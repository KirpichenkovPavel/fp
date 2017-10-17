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

gcd_ :: Integer -> Integer -> Integer
gcd_ first second = 
    if first == 0 then second else
    if second == 0 then first else
    if (first < 0) || (second < 0) then gcd_ (abs first) (abs second) else
    if first `mod` second == 0
    then second
    else 
        if second `mod` first == 0 
        then first
        else
            if first > second 
            then gcd_ (first - second) second
            else gcd_ (second - first) first

-- including borders
squareExists :: Integer -> Integer -> Bool
squareExists first second = 
    let l = if first < second then first else second in
    let r = if first < second then second else first in
    if r < 1 then False else
    if l < 1 then True else 
        ceiling (sqrt (fromIntegral l)) ^ 2 <= r
    
    