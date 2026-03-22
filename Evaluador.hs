module Evaluador where

import Dominio

evaluate :: FunEnv -> Env -> Expr -> Result
evaluate _ _ (Lit n) = Right (VInt n)
evaluate _ _ (BoolLit b) = Right (VBool b)

evaluate _ env (Var x) =
  case lookupEnv x env of
    Just variable -> Right variable
    Nothing -> Left (UndefinedVariable x)

evaluate funEnv env (Add e1 e2) = evalIntBinOp funEnv env (+) e1 e2
evaluate funEnv env (Sub e1 e2) = evalIntBinOp funEnv env (-) e1 e2
evaluate funEnv env (Mul e1 e2) = evalIntBinOp funEnv env (*) e1 e2
evaluate funEnv env (Div e1 e2) = do
  v1 <- evaluate funEnv env e1
  v2 <- evaluate funEnv env e2
  n1 <- expectInt "division" v1
  n2 <- expectInt "division" v2
  if n2 == 0
    then Left DivisionByZero
    else Right (VInt (n1 `div` n2))

evaluate funEnv env (Eq e1 e2) = do
  v1 <- evaluate funEnv env e1
  v2 <- evaluate funEnv env e2
  evalEq v1 v2

evaluate funEnv env (Lt e1 e2) = do
  v1 <- evaluate funEnv env e1
  v2 <- evaluate funEnv env e2
  n1 <- expectInt "comparacion <" v1
  n2 <- expectInt "comparacion <" v2
  Right (VBool (n1 < n2))

evaluate funEnv env (If c t f) = do
  cond <- evaluate funEnv env c
  b <- expectBool cond
  if b
    then evaluate funEnv env t
    else evaluate funEnv env f

evaluate funEnv env (Let x e body) = do
  v <- evaluate funEnv env e
  evaluate funEnv (extendEnv x v env) body

evaluate funEnv env (App name args) =
  case lookupFun name funEnv of
    Nothing -> Left (UndefinedFunction name)
    Just (FunDef _ params body) ->
      if length params /= length args
        then Left (ArityMismatch name (length params) (length args))
        else do
          values <- evalArgs funEnv env args
          let localEnv = bindParams params values
          evaluate funEnv localEnv body

expectInt :: String -> Value -> Either EvalError Int
expectInt _ (VInt n) = Right n
expectInt ctx _ = Left (TypeMismatch ctx)

expectBool :: Value -> Either EvalError Bool
expectBool (VBool b) = Right b
expectBool _ = Left NonBooleanCondition

evalIntBinOp :: FunEnv -> Env -> (Int -> Int -> Int) -> Expr -> Expr -> Result
evalIntBinOp funEnv env op e1 e2 = do
  v1 <- evaluate funEnv env e1
  v2 <- evaluate funEnv env e2
  n1 <- expectInt "operacion aritmetica" v1
  n2 <- expectInt "operacion aritmetica" v2
  Right (VInt (op n1 n2))

evalEq :: Value -> Value -> Result
evalEq (VInt n1) (VInt n2) = Right (VBool (n1 == n2))
evalEq (VBool b1) (VBool b2) = Right (VBool (b1 == b2))
evalEq _ _ = Left (TypeMismatch "comparacion ==")

evalArgs :: FunEnv -> Env -> [Expr] -> Either EvalError [Value]
evalArgs funEnv env =
  foldr step (Right [])
  where
    step expr acc = do
      v <- evaluate funEnv env expr
      vs <- acc
      Right (v : vs)

bindParams :: [String] -> [Value] -> Env
bindParams params values =
  foldr addBinding emptyEnv (zip params values)
  where
    addBinding (name, value) acc = extendEnv name value acc