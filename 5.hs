{-# LANGUAGE Rank2Types #-}
import Rlist
import Data.Functor.Identity

push:: ReverseList a -> a -> ReverseList a
push list item = RCons list item

type MyLens s a = Functor f => (a -> f a) -> s -> f s

-- Индекс с конца по образу и подобию примера из лекции
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

myFoldl :: (b -> a -> b) -> b -> ReverseList a -> b
myFoldl f acc (RCons RNil first) = f acc first
myFoldl f acc (RCons body tail) = f (myFoldl f acc body) tail

-- Прямой индекс, вроде должен работать линейно за 2 прохода
ix :: Integer -> MyLens (ReverseList a) a
ix index f rlist
    | index < 0 = error "rix: negative index"
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
-- {*** Exception: rix: negative index
-- *Main> straightIndexExample1 (fromList [1,2,3,4]) (4)
-- {*** Exception: ix: out of range

straightIndexExample2 :: (Num a, Enum a) => (ReverseList a) -> Integer -> [(ReverseList a)]
straightIndexExample2 list index = 
    ix index (\x -> [1..x]) list
-- *Main> straightIndexExample2 (fromList [1,2,3,4]) 2
-- [{1,2,1,4},{1,2,2,4},{1,2,3,4}]
