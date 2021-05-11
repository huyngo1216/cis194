{-# LANGUAGE FlexibleInstances #-}
module Hw5 where

import ExprT
import Parser
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr ExprT where
  lit = ExprT.Lit
  mul = ExprT.Mul
  add = ExprT.Add

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
eval (ExprT.Lit n) = n
eval (ExprT.Add (ExprT.Lit n) (ExprT.Lit n')) = n + n'
eval (ExprT.Add (ExprT.Lit n) expr) = n + eval expr
eval (ExprT.Add expr (ExprT.Lit n)) = n + eval expr
eval (ExprT.Add expr expr') = eval expr + eval expr'
eval (ExprT.Mul (ExprT.Lit n) (ExprT.Lit n')) = n * n'
eval (ExprT.Mul (ExprT.Lit n) expr) = n * eval expr
eval (ExprT.Mul expr (ExprT.Lit n)) = n * eval expr
eval (ExprT.Mul expr expr') = eval expr * eval expr'

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul s

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Hw5.Lit
  mul = Hw5.Mul
  add = Hw5.Add

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = \_ -> Just n
  mul f g = \m -> let (maybeOp, maybeOp') = (f m, g m)
                  in case maybeOp of
                     Nothing -> Nothing
                     Just n -> case maybeOp' of
                         Nothing -> Nothing
                         Just n' -> Just (n * n')
  add f g = \m -> let (maybeOp, maybeOp') = (f m, g m)
                  in case maybeOp of
                     Nothing -> Nothing
                     Just n -> case maybeOp' of
                         Nothing -> Nothing
                         Just n' -> Just (n + n')


-- sad attempt to not repeat myself
-- combine :: (M.Map String Integer -> Maybe Integer)
--   -> (M.Map String Integer -> Maybe Integer)
--   -> M.Map String Integer
--   -> (Maybe Integer, Maybe Integer)
-- combine f g m = let (maybeOp, maybeOp') = (f m, g m)
--                 in case maybeOp of
--                     Nothing -> (Nothing, Nothing)
--                     Just n -> case maybeOp' of
--                         Nothing -> (Nothing, Nothing)
--                         Just n' -> (Just n, Just n')

-- Tests
-- withVars [("x", 6)] $ add (lit 3) (var "y") -> Nothing
-- withVars [("x", 6)] $ add (lit 3) (var "x") -> Just 9
-- withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")) -> Just 54

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
  