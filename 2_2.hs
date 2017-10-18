fldl:: (b -> a -> b) -> b -> [a] -> b
fldl func acc [] = acc
fldl func acc (h:t) = fldl func (func acc h) t

fldr:: (a -> b -> b) -> b -> [a] -> b
fldr func acc [] = acc
fldr func acc (h:t) = func h (fldr func acc t)
                    
myMap:: (a -> b) -> [a] -> [b]
myMap func list = 
    fldr putFirst [] list
    where putFirst current accList = func current : accList

myConcat:: [a] -> [a] -> [a]
myConcat list1 list2 = 
    fldr putFirst (fldr putFirst [] list2) list1
    where putFirst current accList = current : accList

myFlatMap:: (a -> [b]) -> [a] -> [b]
myFlatMap func list = 
    fldr putList [] list
    where putList current accList = func current `myConcat` accList

myFilter:: (a -> Bool) -> [a] -> [a]
myFilter pred list = 
    fldr putIf [] list
    where putIf current accList = if pred current then current : accList else accList

maxBy:: (a -> Integer) -> [a] -> a
maxBy func list @ [] = error "Don't pass empty list"
maxBy func list @ (h:t) =
    fldl check h list
    where check prev current = if func current > func prev then current else prev

minBy:: (a -> Integer) -> [a] -> a
minBy func list @ [] = error "Don't pass empty list"
minBy func list @ (h:t) =
    fldl check h list
    where check prev current = if func current < func prev then current else prev

myReverse:: [a] -> [a]
myReverse list = 
    fldl putFirst [] list
    where putFirst accList current = current : accList

len:: [a] -> Integer
len list = fldl (\ acc _ -> acc + 1) 0 list

elementAt:: Integer -> [a] -> a
elementAt index list @ ~(h: t) = 
    if index >= len list
        then error "Index is out of range"
        else snd $ fldl hasIndex (0, h) list
    where hasIndex acc current = if fst acc == index then (fst acc + 1, current) else (fst acc + 1, snd acc)

indexOf:: String -> [String] -> Integer
indexOf _ [] = -1
indexOf target list = 
    fst $ fldl strcomp (-1, 0) list
    where strcomp (res, ind) curr = if res > -1 then (res, ind) else if curr == target then (ind, ind+1) else (res, ind+1)
