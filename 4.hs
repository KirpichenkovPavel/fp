data FunMonad a = FunMonad{ fun :: () -> a}

instance Functor FunMonad where
    -- достаем функцию из функтора, делаем a -> b, запаковывваем в функтор.
    fmap function functor = FunMonad (\ () -> function $ (fun functor) ())

instance Applicative FunMonad where
    -- завернуть в функцию
    pure x = FunMonad (\ () -> x)

    -- развернуть, применить, завернуть
    (<*>) (FunMonad function) (FunMonad anotherFunction) = 
        pure $ function () $ anotherFunction ()

instance Monad FunMonad where
    -- применяем преобразующую функцию к завернутой
    (>>=) (FunMonad function) anotherFunction = 
        anotherFunction $ function ()

instance Show a => Show (FunMonad a) where
    show (FunMonad function) = 
        "() -> { " ++ show (function ()) ++ " }"

instance Eq a => Eq (FunMonad a) where
    (==) (FunMonad fst) (FunMonad snd) = fst () == snd ()

-- внутри функция, прибавляющая заданное число
monadPlus:: Num a => a -> FunMonad (a -> a)
monadPlus what = 
    return $ (+ what)

-- для удобства
wrap :: a -> (FunMonad a)
wrap smthg = return smthg

xplusy:: Num a => FunMonad a -> FunMonad a -> FunMonad a
xplusy x y =
    do 
        x' <- x
        y' <- y
        return $ x' + y'

-- применение двух операций к начальному значению
chain:: FunMonad a -> FunMonad (a -> a1) -> FunMonad (a1 -> a2) -> FunMonad a2
chain init f1 f2 =
    do
        init' <- init
        f1' <- f1
        f2' <- f2
        return $ f2' $ f1' $ init'

plus42:: Num a => FunMonad a -> FunMonad a
plus42 x =
    do 
        x' <- x
        y' <- monadPlus 42
        return $ y' x'

-- проверки правил 
test1:: (Eq b) => a -> (a -> FunMonad b) -> Bool
test1 a k = 
    (return a >>= k) == k a

test2:: Eq a => FunMonad a -> Bool
test2 m = 
    (m >>= return) == m

test3:: Eq c => FunMonad a -> (a -> FunMonad b) -> (b -> FunMonad c) -> Bool
test3 m k h = 
    (m >>= (\x -> k x >>= h)) == ((m >>= k) >>= h)

test1':: Bool
test1' = test1 100 (\ x -> return ("Hi, " ++ show x))

test2':: Bool
test2' = test2 $ wrap [1,2,3]

test3':: Bool
test3' = test3 (wrap 9) (\ x -> wrap (x + 5)) (\x -> wrap (x + 7))
