data WeirdPeanoNumber = 
    Zero 
    | Succ (WeirdPeanoNumber ) 
    | Pred (WeirdPeanoNumber )

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b    
    (==) (Pred a) (Pred b) = a == b
    (==) this other = False

instance Ord WeirdPeanoNumber where
    (<=) Zero (Pred _) = False
    (<=) Zero other = True
    (<=) (Succ a) (Succ b) = a <= b
    (<=) (Succ a) other = False
    (<=) (Pred a) (Pred b) = a <= b
    (<=) (Pred a) other = True

instance Num WeirdPeanoNumber where
    (+) this Zero = this
    (+) Zero other = other
    (+) (Succ a) (Succ b) = Succ (Succ a) + b
    (+) (Succ a) (Pred b) = a + b
    (+) (Pred a) (Pred b) = Pred (Pred a) + b
    (+) (Pred a) (Succ b) = a + b    
    
    negate Zero = Zero
    negate (Succ a) = Pred (negate a)
    negate (Pred a) = Succ (negate a)
    
    abs (Pred a) = Succ (abs a)
    abs a = a

    signum Zero = Zero
    signum (Succ a) = Succ Zero
    signum (Pred a) = Pred Zero

    fromInteger val | (val == 0) = Zero
                    | (val > 0) = Succ $ fromInteger (val - 1)
                    | (val < 0) = Pred $ fromInteger (val + 1)

    (*) Zero other = Zero
    (*) other Zero = Zero
    (*) (Succ Zero) other = other
    (*) this (Succ Zero) = this
    (*) (Succ a) (Succ b) = a * Succ b + Succ b
    (*) this @ (Pred a) other @ (Pred b) = negate this * negate other
    (*) this @ (Succ a) other @ (Pred b) = negate $ this * negate other
    (*) this @ (Pred a) other @ (Succ b) = negate $ negate this * other

instance Real WeirdPeanoNumber where
    toRational Zero = toRational 0
    toRational (Succ a) = toRational (toRational a + 1)
    toRational (Pred a) = toRational (toRational a + 1)

instance Enum WeirdPeanoNumber where
    toEnum int | (int == 0) = Zero
               | (int > 0) = Succ $ toEnum (int - 1)
               | (int < 0) = Pred $ toEnum (int + 1)

    fromEnum Zero = 0
    fromEnum (Succ a) = fromEnum a + 1
    fromEnum (Pred a) = fromEnum a - 1

instance Integral WeirdPeanoNumber where
    toInteger Zero = toInteger 0
    toInteger (Succ a) = toInteger (toInteger a + 1)
    toInteger (Pred a) = toInteger (toInteger a - 1)

    -- quotRem num den = 
    --     let quotInt n d = quot (toInteger n) (toInteger d) in
    --     let remInt n d = rem (toInteger n) (toInteger d) in
    --     (fromInteger $ quotInt num den , fromInteger $ remInt num den)
    quotRem num den =
        move num den Zero Zero
        where 
            move n Zero q r = error "Division by zero"
            move Zero d q r = (q, r)
            move (Succ n) (Succ d) q r =
                if d == r
                    then move n (Succ d) (Succ q) Zero
                    else move n (Succ d) q (Succ r)
            move (Succ n) (Pred d) q r =
                if d + r == Zero
                    then move n (Pred d) (Pred q) Zero
                    else move n (Pred d) q (Succ r)
            move (Pred n) (Succ d) q r =
                if d + r == Zero
                    then move n (Succ d) (Pred q) Zero
                    else move n (Succ d) q (Pred r)
            move (Pred n) (Pred d) q r =
                if d == r
                    then move n (Pred d) (Succ q) Zero
                    else move n (Pred d) q (Pred r)

instance Show WeirdPeanoNumber where
    show a = show (toInteger a) ++ "p"

peano:: WeirdPeanoNumber -> WeirdPeanoNumber
peano = id

test:: WeirdPeanoNumber
test = peano $ foldl (\ acc val -> acc + val `mod` 3) 6 [-20..14]
