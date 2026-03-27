module Main where

import Dominio
import Evaluador (evaluate)

-- Funciones predefinidas disponibles en el interprete
funcionesBase :: FunEnv
funcionesBase
  = registerFun factorial
  $ registerFun suma
  $ registerFun maximo
  $ emptyFunEnv
  where
    factorial = FunDef "factorial" ["n"]
      (If (Eq (Var "n") (Lit 0))
          (Lit 1)
          (Mul (Var "n") (App "factorial" [Sub (Var "n") (Lit 1)])))

    suma = FunDef "suma" ["x", "y"]
      (Add (Var "x") (Var "y"))

    maximo = FunDef "maximo" ["x", "y"]
      (If (Lt (Var "x") (Var "y"))
          (Var "y")
          (Var "x"))

-- Pipeline puro: parsea y evalua usando las funciones base
evalInput :: String -> Either EvalError Value
evalInput = (>>= evaluate funcionesBase emptyEnv) . parseExpr

-- Convierte el resultado a texto
runPipeline :: String -> String
runPipeline = either showError showValue . evalInput

-- Lee desde archivo si la entrada empieza con ':file', sino la usa directo
readSource :: String -> IO String
readSource line =
  case words line of
    ":file" : pathParts -> readFile (unwords pathParts)
    _ -> return line

main :: IO ()
main = do
  putStrLn "Interprete funcional - Proyecto 2"
  putStrLn "Funciones disponibles: factorial, suma, maximo"
  putStrLn "Ingresa una expresion o ':file ruta':"
  entry <- getLine
  source <- readSource entry
  putStrLn (runPipeline source)

-- Parser de expresiones

parseExpr :: String -> Either EvalError Expr
parseExpr input = do
  (expr, remaining) <- parseTokens (tokenize input)
  case remaining of
    [] -> Right expr
    _  -> Left (TypeMismatch "hay tokens sobrantes")

-- Separa la cadena en tokens aislando parentesis
tokenize :: String -> [String]
tokenize = words . concatMap split
  where
    split '(' = " ( "
    split ')' = " ) "
    split c   = [c]

-- Parser recursivo principal
parseTokens :: [String] -> Either EvalError (Expr, [String])
parseTokens [] = Left (TypeMismatch "entrada vacia")
parseTokens ("(" : op : tokens) = parseGrouped op tokens
parseTokens (tok : tokens) = parseAtom tok tokens

-- Parsea literales y variables
parseAtom :: String -> [String] -> Either EvalError (Expr, [String])
parseAtom "true"  tokens = Right (BoolLit True, tokens)
parseAtom "false" tokens = Right (BoolLit False, tokens)
parseAtom tok     tokens =
  case reads tok of
    [(n, "")] -> Right (Lit n, tokens)
    _         -> Right (Var tok, tokens)

-- Elige el parser segun el operador
parseGrouped :: String -> [String] -> Either EvalError (Expr, [String])
parseGrouped "add" = parseBin Add
parseGrouped "sub" = parseBin Sub
parseGrouped "mul" = parseBin Mul
parseGrouped "div" = parseBin Div
parseGrouped "eq"  = parseBin Eq
parseGrouped "lt"  = parseBin Lt
parseGrouped "if"  = parseIf
parseGrouped "let" = parseLet
parseGrouped "app" = parseApp
parseGrouped _     = \_ -> Left (TypeMismatch "operacion no soportada")

-- Parsea una operacion binaria
parseBin :: (Expr -> Expr -> Expr) -> [String] -> Either EvalError (Expr, [String])
parseBin ctor tokens = do
  (e1, t1) <- parseTokens tokens
  (e2, t2) <- parseTokens t1
  t3       <- consumeClose t2
  Right (ctor e1 e2, t3)

-- Parsea if con condicion, rama verdadera y rama falsa
parseIf :: [String] -> Either EvalError (Expr, [String])
parseIf tokens = do
  (c, t1) <- parseTokens tokens
  (t, t2) <- parseTokens t1
  (f, t3) <- parseTokens t2
  t4      <- consumeClose t3
  Right (If c t f, t4)

-- Parsea let con nombre, expresion y cuerpo
parseLet :: [String] -> Either EvalError (Expr, [String])
parseLet [] = Left (TypeMismatch "let incompleto")
parseLet (name : t1) = do
  (e,    t2) <- parseTokens t1
  (body, t3) <- parseTokens t2
  t4         <- consumeClose t3
  Right (Let name e body, t4)

-- Parsea una llamada a funcion con sus argumentos
parseApp :: [String] -> Either EvalError (Expr, [String])
parseApp [] = Left (TypeMismatch "app incompleto")
parseApp (fname : tokens) = do
  (args, t1) <- parseArgs tokens
  Right (App fname args, t1)

-- Parsea argumentos hasta encontrar el cierre
parseArgs :: [String] -> Either EvalError ([Expr], [String])
parseArgs []            = Left (TypeMismatch "falta cerrar app")
parseArgs (")" : tokens) = Right ([], tokens)
parseArgs tokens = do
  (arg,  t1) <- parseTokens tokens
  (more, t2) <- parseArgs t1
  Right (arg : more, t2)

-- Consume el parentesis de cierre
consumeClose :: [String] -> Either EvalError [String]
consumeClose []         = Left (TypeMismatch "fin inesperado")
consumeClose (")" : xs) = Right xs
consumeClose _          = Left (TypeMismatch "se esperaba cierre de parentesis")

-- Ejemplos de uso:
-- (app factorial 5)          -> 120
-- (app suma x y)             -> Error: variable 'x' no definida
-- (app maximo 3 9)           -> 9
-- (add 3 4)                  -> 7
-- (if (lt 2 5) 100 200)      -> 100
-- (let n 6 (app factorial n)) -> 720
-- :file input.txt