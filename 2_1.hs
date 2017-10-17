data BinaryTree = 
    EmptyTree 
    | Leaf Integer
    | Node Integer BinaryTree BinaryTree deriving(Show,Eq)

emptyTree:: BinaryTree
emptyTree = EmptyTree

insert:: BinaryTree -> Integer -> BinaryTree
insert (EmptyTree) new = Leaf new
insert leaf @ (Leaf val) new = 
    if new < val then Node val (Leaf new) EmptyTree else Node val EmptyTree (Leaf new)
insert node @ (Node val left right) new =
    if new < val 
        then Node val (insert left new) right 
        else Node val left (insert right new)

(+=):: BinaryTree -> Integer -> BinaryTree
(+=) tree new = tree `insert` new 

containsElement:: BinaryTree -> Integer -> Bool
containsElement (EmptyTree) target = False
containsElement (Leaf val) target = val == target
containsElement (Node val left right) target = 
    if val == target then True else
        if target < val then containsElement left target else containsElement right target

has tree target = containsElement tree target

rightMost:: BinaryTree -> Integer
rightMost (Leaf v) = v
rightMost (Node v l r) = 
    case r of 
        (EmptyTree) -> v
        (Leaf v') -> v'
        (Node v' l' r') -> rightMost r

isEmpty:: BinaryTree -> Bool
isEmpty bt = 
    case bt of 
        (EmptyTree) -> True
        otherwise -> False

isLeaf:: BinaryTree -> Bool
isLeaf bt = case bt of 
    (Leaf _) -> True
    otherwise -> False

isNode:: BinaryTree -> Bool
isNode bt = case bt of 
    (Node _ _ _) -> True
    otherwise -> False

nearestGE:: BinaryTree -> Integer -> Maybe Integer
nearestGE (EmptyTree) target = Nothing
nearestGE (Leaf val) target = if val >= target then Just val else Nothing
nearestGE (Node val left right) target =     
    if val < target then nearestGE right target else        
        if (isEmpty left) || (rightMost left < target) then Just val else
            case left of 
                (Leaf v) -> Just v
                otherwise -> nearestGE left target

shifted:: BinaryTree -> Integer -> BinaryTree
shifted (Leaf val) target = EmptyTree
shifted (Node val left right) target = 
    if val == target 
        then right -- if value is nearestGE (target), then left is empty
        else if target < val 
            then Node val (shifted left target) right
            else Node val left (shifted right target)

remove:: BinaryTree -> Integer -> BinaryTree
remove (EmptyTree) target = EmptyTree
remove leaf @ (Leaf val) target = if val == target then EmptyTree else leaf
remove node @ (Node val left right) target =    
    if not (has node target) then node else
    if val == target then 
        case (nearestGE right val) of
            (Nothing) -> left
            (Just int) -> if (isEmpty left) && (isLeaf right)
                then right
                else Node int left (shifted right int)            
    else if target < val 
        then Node val (remove left target) right 
        else Node val left (remove right target)    

(-=):: BinaryTree -> Integer -> BinaryTree
(-=) tree new = tree `remove` new 

treeFromList:: [Integer] -> BinaryTree
treeFromList [] = emptyTree
treeFromList (h:t) = treeFromList t += h


listFromTree:: BinaryTree -> [Integer]
listFromTree (EmptyTree) = []
listFromTree (Leaf val) = [val]
listFromTree (Node val left right) = (listFromTree left) ++ [val] ++ (listFromTree right)
