{-# LANGUAGE Rank2Types #-}
import Rlist
import Term
import Data.Functor.Identity

push:: ReverseList a -> a -> ReverseList a
push list item = RCons list item

type MyLens s a = Functor f => (a -> f a) -> s -> f s

-- Индекс с конца
rix :: Integer -> MyLens (ReverseList a) a
rix index f rlist 
    | index < 0 = error "rix: negative index"
    | RNil <- rlist = error "rix: out of range"
    | (RCons body tail) <- rlist = 
        if index == 0 
            then (body `push`) <$> f tail
            else (`push` tail) <$> rix (index - 1) f body

over :: MyLens s a -> (a -> a) -> s -> s
over lens f = runIdentity . lens (Identity . f)

reverseIndexExample1 :: Num a => (ReverseList a) -> (ReverseList a)
reverseIndexExample1 list = 
    over (rix 1) (\ x -> x + 15) list
-- *Main> reverseIndexExample1 $ fromList[1,2,3,4]
-- {1,2,18,4}

reverseIndexExample2 :: Num a => (ReverseList a) -> Integer ->  [(ReverseList a)]
reverseIndexExample2 list index = 
    rix index (\x -> [x-1, x+3, 0]) list
-- *Main> reverseIndexExample2 (fromList[1,2,3,4]) 0
-- [{1,2,3,3},{1,2,3,7},{1,2,3,0}]

reverseIndexExample3 :: Num a => (ReverseList [a]) -> Integer -> (ReverseList [a])
reverseIndexExample3 list index =
    over (rix index) (\ (h:t) -> (h-1:h:h+1:t)) list
-- *Main> reverseIndexExample3 (fromList[[1],[2],[3],[4]]) 2
-- {[1],[1,2,3],[3],[4]}

-- Свертка для прямого индекса
myFoldl :: (b -> a -> b) -> b -> ReverseList a -> b
myFoldl f acc (RCons RNil first) = f acc first
myFoldl f acc (RCons body tail) = f (myFoldl f acc body) tail

-- Прямой индекс, должен работать за 2 прохода
-- За первый проход (split) формируем части: до элемента, сам элемент, после элемента
-- За втророй проход (apply) применяем функцию и добавляем концы
ix :: Integer -> MyLens (ReverseList a) a
ix index f rlist
    | index < 0 = error "ix: negative index"
    | otherwise =
        apply $ myFoldl split (index, RNil, RNil, RNil) rlist where
            apply (c, left, RNil, right) = error "ix: out of range"
            apply (c, left, RCons RNil item, (RCons b t)) = 
                (`push` t) <$> apply (c, left, RCons RNil item, b)
            apply (c, left, RCons RNil item, RNil) = (left `push`) <$> f item                
            split (ind, l, i, r) el 
                | ind > 0 = (ind - 1, RCons l el, i, r)
                | ind == 0 = (ind - 1, l, RCons RNil el, r)
                | ind < 0 = (ind - 1, l, i, RCons r el)

straightIndexExample1 :: Num a => ReverseList a -> Integer -> ReverseList a
straightIndexExample1 list index = 
    over (ix index) (\x -> x + 15) list
-- *Main> straightIndexExample1 (fromList [1,2,3,4]) 0
-- {16,2,3,4}
-- *Main> straightIndexExample1 (fromList [1,2,3,4]) (3)
-- {1,2,3,19}
-- *Main> straightIndexExample1 (fromList [1,2,3,4]) (-1)
-- {*** Exception: ix: negative index
-- *Main> straightIndexExample1 (fromList [1,2,3,4]) (4)
-- {*** Exception: ix: out of range

straightIndexExample2 :: (Num a, Enum a) => (ReverseList a) -> Integer -> [(ReverseList a)]
straightIndexExample2 list index = 
    ix index (\x -> [1..x]) list
-- *Main> straightIndexExample2 (fromList [1,2,3,4]) 2
-- [{1,2,1,4},{1,2,2,4},{1,2,3,4}]

-- Первый элемент обратного списка
first :: MyLens (ReverseList a) a
first f rlist 
    | RNil <- rlist = error "rix: out of range"
    | (RCons RNil head) <- rlist = (RNil `push`) <$> f head
    | (RCons body tail) <- rlist = (`push` tail) <$> first f body

-- *Main> over first (\x -> x + 25) (fromList [1,2,3,4])
-- {26,2,3,4}
-- *Main> first (\x -> [x - 1, x + 1]) (fromList [1,2,3,4])
-- [{0,2,3,4},{2,2,3,4}]

-- N-я встреченная переменная с указанным имененем
var :: Integer -> String -> (Term -> Term) -> Term -> Term
var index name f term = 
    let rep (term, ind) = if ind < 0 
        then (term, ind)
        else case term of
            (Variable n) -> if n == name
                then if ind == 0
                    then (f (Variable name), ind - 1)
                    else (term, ind - 1)
                else (term, ind)
            (IntConstant int) -> (term, ind)
            (UnaryTerm op t) -> 
                (UnaryTerm op (fst $ rep (t, ind)), snd $ rep (t, ind))
            (BinaryTerm l op r) -> 
                (BinaryTerm (fst $ rep (l, ind)) op (fst $ rep (r, snd $ rep (l, ind))), snd $ rep (r, snd $ rep (l, ind))) in
    fst $ rep (term, index)
-- *Main> let v1 = Variable "x1"
-- *Main> let v2 = Variable "x2"
-- *Main> let expr = v1 <+> v2 <@> v1 <-> v1 <-> v2
-- *Main> expr
-- ((("x1"+("x2"*"x1"))-"x1")-"x2")
-- *Main> let expr' = IntConstant 42 <+> expr <-> IntConstant 42
-- *Main> expr'
-- ((42+((("x1"+("x2"*"x1"))-"x1")-"x2"))-42)
-- Заменить вторую слева переменную х1 в выражении expr' на минус х1
-- *Main> var 1 "x1" (\ x -> UnaryTerm Neg x) expr'
-- ((42+((("x1"+("x2"*(-"x1")))-"x1")-"x2"))-42)
-- Заменить третью переменную х1 на сумму двух таких переменных
-- *Main> var 2 "x1" (\ x -> x <+> x) expr'
-- ((42+((("x1"+("x2"*"x1"))-("x1"+"x1"))-"x2"))-42)

-- N-я по счету переменная или константа
termix :: Integer -> (Term -> Term) -> Term -> Term
termix index f term = 
    let rep (term, ind) = if ind < 0 
        then (term, ind)
        else case term of
            (Variable n) -> if ind == 0 
                then (f term, ind - 1)
                else (term, ind - 1)
            (IntConstant int) -> if ind == 0 
                then (f term, ind - 1)
                else (term, ind - 1)
            (UnaryTerm op t) -> 
                (UnaryTerm op (fst $ rep (t, ind)), snd $ rep (t, ind))
            (BinaryTerm l op r) -> 
                (BinaryTerm (fst $ rep (l, ind)) op (fst $ rep (r, snd $ rep (l, ind))), snd $ rep (r, snd $ rep (l, ind))) in
    fst $ rep (term, index)

doThings :: Term -> Term
doThings term =
    case term of 
        Variable name -> IntConstant 42
        IntConstant int -> if int == 42 
            then IntConstant $ int + 1
            else IntConstant 42
        otherwise -> term
-- *Main> let v1 = Variable "x1"
-- *Main> let v2 = Variable "x2"
-- *Main> let expr = v1 <+> v2 <@> v1 <-> v1 <-> v2
-- *Main> let expr' = IntConstant 42 <+> expr <-> IntConstant 5
-- *Main> expr'
-- ((42+((("x1"+("x2"*"x1"))-"x1")-"x2"))-5)
-- Обработать первый терм
-- *Main> termix 0 doThings expr'
-- ((43+((("x1"+("x2"*"x1"))-"x1")-"x2"))-5)
-- Обработать четвертый терм
-- *Main> termix 3 doThings expr'
-- ((42+((("x1"+("x2"*42))-"x1")-"x2"))-5)
-- Обработать седьмой терм
-- *Main> termix 6 doThings expr'
-- ((42+((("x1"+("x2"*"x1"))-"x1")-"x2"))-42)
