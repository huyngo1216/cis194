import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

instance Expr Bool where
    lit n = n > 0
    mul = (&&)
    add = (||)

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr MinMax where
    lit = MinMax
    mul = min
    add = max

instance Expr Mod7 where
    lit n = Mod7 $ n `mod` 7
    mul (Mod7 n) (Mod7 m) = Mod7 (m * n `mod` 7)
    add (Mod7 n) (Mod7 m) = Mod7 ((m + n) `mod` 7)

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add (Lit n) (Lit n')) = n + n'
eval (Add (Lit n) expr) = n + eval expr
eval (Add expr (Lit n)) = n + eval expr
eval (Add expr expr') = eval expr + eval expr'
eval (Mul (Lit n) (Lit n')) = n * n'
eval (Mul (Lit n) expr) = n * eval expr
eval (Mul expr (Lit n)) = n * eval expr
eval (Mul expr expr') = eval expr * eval expr'

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s
