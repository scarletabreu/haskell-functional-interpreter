module Dominio where

data Expr
  = Lit Int
  | BoolLit Bool
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr        -- puede fallar si el divisor es 0
  | Eq  Expr Expr
  | Lt  Expr Expr
  | If  Expr Expr Expr
  | App String [Expr]    -- llamada a funcion con sus argumentos
  | Let String Expr Expr

data Value
  = VInt  Int
  | VBool Bool
  | VFun  [String] Expr Env  -- closure: params, cuerpo y entorno

data EvalError
  = UndefinedVariable String
  | DivisionByZero
  | TypeMismatch String
  | UndefinedFunction String
  | ArityMismatch String Int Int
  | NonBooleanCondition

-- entorno: lista de pares nombre-valor
data Env = Env [(String, Value)]

emptyEnv :: Env
emptyEnv = Env []

extendEnv :: String -> Value -> Env -> Env
extendEnv x v (Env pairs) = Env ((x, v) : pairs)

lookupEnv :: String -> Env -> Maybe Value
lookupEnv _ (Env []) = Nothing
lookupEnv x (Env ((k, v) : xs))
  | x == k    = Just v
  | otherwise = lookupEnv x (Env xs)

-- una funcion tiene nombre, lista de parametros y un cuerpo
data FunDef = FunDef String [String] Expr

data FunEnv = FunEnv [FunDef]

emptyFunEnv :: FunEnv
emptyFunEnv = FunEnv []

registerFun :: FunDef -> FunEnv -> FunEnv
registerFun fd (FunEnv fds) = FunEnv (fd : fds)

lookupFun :: String -> FunEnv -> Maybe FunDef
lookupFun _ (FunEnv []) = Nothing
lookupFun name (FunEnv (FunDef n ps b : xs))
  | name == n = Just (FunDef n ps b)
  | otherwise = lookupFun name (FunEnv xs)

data Program = Program FunEnv Expr

-- el resultado de evaluar es un valor o un error
type Result = Either EvalError Value

-- junta una lista de strings con comas
commaSep :: [String] -> String
commaSep [] = ""
commaSep [x] = x
commaSep (x:xs) = x ++ ", " ++ commaSep xs

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (BoolLit b) = show b
showExpr (Var x) = x
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
showExpr (Eq e1 e2)  = "(" ++ showExpr e1 ++ " == " ++ showExpr e2 ++ ")"
showExpr (Lt e1 e2)  = "(" ++ showExpr e1 ++ " < " ++ showExpr e2 ++ ")"
showExpr (If c t f) = "if " ++ showExpr c ++ " then " ++ showExpr t ++ " else " ++ showExpr f
showExpr (App f args) = f ++ "(" ++ commaSep (map showExpr args) ++ ")"
showExpr (Let x e body) = "let " ++ x ++ " = " ++ showExpr e ++ " in " ++ showExpr body

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool b) = show b
showValue (VFun ps _ _) = "<funcion/" ++ show (length ps) ++ ">"

showError :: EvalError -> String
showError (UndefinedVariable x) = "Error: variable '" ++ x ++ "' no definida."
showError DivisionByZero = "Error: division por cero."
showError (TypeMismatch ctx) = "Error de tipo en: " ++ ctx
showError (UndefinedFunction f) = "Error: funcion '" ++ f ++ "' no definida."
showError (ArityMismatch f esp rec) = "Error: '" ++ f ++ "' esperaba " ++ show esp ++ " args, recibio " ++ show rec ++ "."
showError NonBooleanCondition = "Error: la condicion del if no es booleana."

showFunDef :: FunDef -> String
showFunDef (FunDef name params body) = name ++ "(" ++ commaSep params ++ ") = " ++ showExpr body

eqExpr :: Expr -> Expr -> Bool
eqExpr (Lit n1) (Lit n2) = n1 == n2
eqExpr (BoolLit b1) (BoolLit b2) = b1 == b2
eqExpr (Var x1) (Var x2) = x1 == x2
eqExpr (Add a1 b1) (Add a2 b2) = eqExpr a1 a2 && eqExpr b1 b2
eqExpr (Sub a1 b1) (Sub a2 b2) = eqExpr a1 a2 && eqExpr b1 b2
eqExpr (Mul a1 b1) (Mul a2 b2) = eqExpr a1 a2 && eqExpr b1 b2
eqExpr (Div a1 b1) (Div a2 b2) = eqExpr a1 a2 && eqExpr b1 b2
eqExpr (Eq a1 b1) (Eq a2 b2) = eqExpr a1 a2 && eqExpr b1 b2
eqExpr (Lt a1 b1) (Lt a2 b2) = eqExpr a1 a2 && eqExpr b1 b2
eqExpr (If c1 t1 f1) (If c2 t2 f2) = eqExpr c1 c2 && eqExpr t1 t2 && eqExpr f1 f2
eqExpr (App f1 xs1) (App f2 xs2) = f1 == f2 && all (\(a, b) -> eqExpr a b) (zip xs1 xs2)
eqExpr (Let x1 e1 b1) (Let x2 e2 b2) = x1 == x2 && eqExpr e1 e2 && eqExpr b1 b2
eqExpr _ _ = False

eqError :: EvalError -> EvalError -> Bool
eqError (UndefinedVariable x1) (UndefinedVariable x2) = x1 == x2
eqError DivisionByZero DivisionByZero = True
eqError (TypeMismatch c1) (TypeMismatch c2) = c1 == c2
eqError (UndefinedFunction f1) (UndefinedFunction f2) = f1 == f2
eqError (ArityMismatch f1 e1 r1) (ArityMismatch f2 e2 r2) = f1 == f2 && e1 == e2 && r1 == r2
eqError NonBooleanCondition NonBooleanCondition = True
eqError _ _ = False