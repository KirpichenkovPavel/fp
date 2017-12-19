module Rlist where

data ReverseList a =
    RNil 
    | RCons (ReverseList a) a

instance (Show a) => Show (ReverseList a) where
    show rlist = 
        let 
        showElements RNil = ""
        showElements (RCons RNil el) = show el
        showElements (RCons inner el) = showElements inner ++ "," ++ show el in
        "{" ++ showElements rlist ++ "}"


toList:: ReverseList a -> [a]
toList rev = 
    let listed (RNil) = []
        listed (RCons i e) = e : listed i in
    reverse $ listed rev

fromList:: [a] -> ReverseList a
fromList list = foldl (\ acc el -> RCons acc el) RNil list

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) (RCons a1 b1) (RCons a2 b2) = a1 == a2 && b1 == b2
    (==) _ _ = False

instance (Ord a) => Ord (ReverseList a) where
    (<=) this other = toList this <= toList other

instance Monoid (ReverseList a) where
    mempty = RNil    
    mappend RNil other = other
    mappend this RNil = this
    mappend left right =
        foldl (\ acc el -> RCons acc el) left (toList right)

instance Functor ReverseList where
    fmap func RNil = RNil
    fmap func (RCons inner el) = RCons (fmap func inner) (func el)
