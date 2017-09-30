data BinaryOperator = Plus | Minus | Mult deriving(Show,Eq)
data UnaryOperator = Neg deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }
            | UnaryTerm{ uop :: UnaryOperator, rhv :: Term }
            | BinaryTerm{ lhv :: Term, op :: BinaryOperator, rhv :: Term } deriving(Show,Eq)
(<+>) left right = BinaryTerm left Plus right
(<->) left right = BinaryTerm left Minus right
-- <*> conflicts with Prelude
neg term = UnaryTerm Neg term
(<@>) left right = BinaryTerm left Mult right
infixl 3 <+>
infixl 3 <->
infixl 7 <@>
replaceVar (Variable str) name new = 
	if str == name then new else Variable str
replaceVar (IntConstant int) name new =
	IntConstant int
replaceVar (UnaryTerm op term) name new = 
	UnaryTerm op (replaceVar term name new)
replaceVar (BinaryTerm left expr right) name new = 
	BinaryTerm (replaceVar left name new) expr (replaceVar right name new)

