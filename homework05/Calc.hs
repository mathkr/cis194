{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- CIS 194 Homework 5
--

module Calc where

import Control.Applicative
import Parser (parseExp)
import qualified Data.Map as M
import qualified StackVM  as S
import qualified ExprT    as E (ExprT(..))  -- importing as E because
                                            -- of nameclash with VarExprT

----------------
-- Exercise 1 --
----------------

eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add e1 e2) = (eval e1) + (eval e2)
eval (E.Mul e1 e2) = (eval e1) * (eval e2)

----------------
-- Exercise 2 --
----------------

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

----------------
-- Exercise 3 --
----------------

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr E.ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

----------------
-- Exercise 4 --
----------------

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMinMax :: Maybe MinMax
testMinMax = testExp

testMod7 :: Maybe Mod7
testMod7 = testExp

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

-- Deriving Ord for MinMax to be able to just use 'max'
-- and 'min' in Expr instance definition
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 a) (Mod7 b) = lit $ a + b
    mul (Mod7 a) (Mod7 b) = lit $ a * b

----------------
-- Exercise 5 --
----------------

testProgram :: Maybe S.Program
testProgram = testExp

instance Expr S.Program where
    lit = (: []) . S.PushI
    add e1 e2 = e1 ++ e2 ++ [S.Add]
    mul e1 e2 = e1 ++ e2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile s = return s >>= parseExp lit add mul

----------------
-- Exercise 6 --
----------------

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

-- Currying M.lookup with a Key (String in this case) turns it into a function
-- from a Map into a Maybe Value
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- Expression instance using the fact that Maybe is a Functor and an Applicative
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n     = (\_ -> Just n)
    add f1 f2 = (\m -> (+) <$> (f1 m) <*> (f2 m))
    mul f1 f2 = (\m -> (*) <$> (f1 m) <*> (f2 m))

-- Testing function given in assignment
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs
