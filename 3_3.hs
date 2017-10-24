newtype PSet a = PSet{ contains :: (a -> Bool)}

instance Monoid (PSet a) where
    -- Если множество пустое, то предикат отбрасывает любое значение
    mempty = PSet (\ _ -> False)
    -- У объединения множеств проверяется, что элемент удовлетворяет
    -- хотя бы одному из предикатов
    mappend (PSet p1) (PSet p2) = PSet (\ el -> p1 el || p2 el)
-- _______________________________________________________________________ --


-- Если множество ограниченное, упорядоченное и счетное, можно свести 
-- проверку предиката к проверке элементов. В данном случае строится
-- карта элементов, каждому значению сопоставляется логическое значение принадлежности.
-- При соединении множеств карты обходятся, если хотя бы в одной текущий элемент
-- отмечен как принадлежащий, то он записывается в новую карту.
-- Этот вариант, как и следующий, неэффективен, зависает на Int,
-- но на множестве Bool работает.
-- instance (Ord a, Enum a, Bounded a) => Monoid (PSet a) where
--     mempty = PSet (\ _ -> False)

--     mappend (PSet p1) (PSet p2) = 
--         let traverse p [] = []
--             traverse p (h:t) = (h, p h) : traverse p t in
--         let unite [] [] = []
--             unite (h1:t1) (h2:t2) = 
--                 (fst h1, (snd h1 || snd h2)) : unite t1 t2 in
--         let check el ((val, bool):t') = if val == el then bool else check el t' in
--         PSet (\ it -> check it (unite (traverse p1 rng) (traverse p2 rng)))
--         where
--             rng = [minBound..maxBound]
-- _______________________________________________________________________ --

-- В этом варианте промежуточный список содержит только содержащиеся в множестве элементы.
-- При объединении множеств списки сливаются. Все еще обходятся все значения домена.
-- instance (Ord a, Enum a, Bounded a) => Monoid (PSet a) where
--     mempty = PSet (\ _ -> False)

--     mappend (PSet p1) (PSet p2) = 
--         let traverse p list = foldl (\ acc el -> if p el then el:acc else acc) [] list in
--         let merge [] other = other
--             merge this [] = this
--             merge (h1:t1) (h2:t2) = 
--                 if h1 == h2 then 
--                     h1 : merge t1 t2 
--                     else if h1 < h2 
--                         then h1 : merge t1 (h2:t2) 
--                         else h2 : merge (h1:t1) t2 in
--         PSet (\ it -> elem it (merge (traverse p1 rng) (traverse p2 rng)))
--         where
--             rng = [minBound..maxBound]
-- _______________________________________________________________________ --

trueSet :: (PSet Bool)
trueSet = PSet (\ x -> x)

falseSet :: (PSet Bool)
falseSet = PSet (\ x -> not x)

completeSet :: (PSet Bool)
completeSet = trueSet `mappend` falseSet

emptySet :: (PSet Bool)
emptySet = mempty

test:: String
test = 
    if check 
        then "Test passed" 
        else "Test failed"
    where 
        check = ts True && not (ts False) && not (fs True) && fs False && cs True && cs False && not (es True) && not (es False)
        ts = contains trueSet
        fs = contains falseSet
        cs = contains completeSet
        es = contains emptySet

-- Functor реализовать не получается. 
-- Функция из fmap отображает (a -> b). Идеологически хотим либо преобразовать
-- результат обратно в тип a, чтобы подходил под старый предикат, либо изменить предикат,
-- чтобы принимал тип b и сохранял смысл в новом домене. Функкция для fmap в общем случае
-- может быть необратима. Кроме того непонятно, как в принципе вычислить обратную 
-- функцию к произвольной. Первый вариант не срабатывает. Второй вариант не подходит,
-- т.к. не видно обобщенной связи между преобразованием элементов и преобразованием 
-- описывающего их предиката. Функция для fmap может, например, вообще игнорировать аргумент,
-- изнутри этого не выяснить.
