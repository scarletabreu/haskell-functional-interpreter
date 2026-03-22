module Main where

import Dominio
import Evaluador (evaluate)


parseExpr :: String -> Either EvalError Expr
parseExpr input = do
  (expr, rest) <- parseTokens (tokenize input)
  case rest of
    [] -> Right expr
    _ -> Left (TypeMismatch "hay tokens sobrantes")

-- Separa la cadena en tokens (funciones definidas), aislando parentesis.
tokenize :: String -> [String]
tokenize = words . concatMap split
  where
    split '(' = " ( "
    split ')' = " ) "
    split c = [c]

-- Parser recursivo principal sobre la lista de tokens (funciones definidas).
parseTokens :: [String] -> Either EvalError (Expr, [String])
parseTokens [] = Left (TypeMismatch "entrada vacia")
parseTokens ("(" : op : rest) = parseGrouped op rest
parseTokens (tok : rest) = parseAtom tok rest

-- Parsea booleanos, enteros o variables.
parseAtom :: String -> [String] -> Either EvalError (Expr, [String])
parseAtom "true" rest = Right (BoolLit True, rest)
parseAtom "false" rest = Right (BoolLit False, rest)
parseAtom tok rest =
  case reads tok of
    [(n, "")] -> Right (Lit n, rest)
    _ -> Right (Var tok, rest)

  -- Elige el parser segun el nombre de la operacion.
parseGrouped :: String -> [String] -> Either EvalError (Expr, [String])
parseGrouped "add" = parseBin Add
parseGrouped "sub" = parseBin Sub
parseGrouped "mul" = parseBin Mul
parseGrouped "div" = parseBin Div
parseGrouped "eq" = parseBin Eq
parseGrouped "lt" = parseBin Lt
parseGrouped "if" = parseIf
parseGrouped "let" = parseLet
parseGrouped "app" = parseApp
parseGrouped _ = \_ -> Left (TypeMismatch "operacion no soportada")

-- Parsea una operacion binaria y consume el parentesis de cierre.
parseBin :: (Expr -> Expr -> Expr) -> [String] -> Either EvalError (Expr, [String])
parseBin ctor tokens = do
  (e1, t1) <- parseTokens tokens
  (e2, t2) <- parseTokens t1
  t3 <- consumeParentesis ")" t2
  Right (ctor e1 e2, t3)

-- Parsea una expresion condicional if.
parseIf :: [String] -> Either EvalError (Expr, [String])
parseIf tokens = do
  (c, t1) <- parseTokens tokens
  (t, t2) <- parseTokens t1
  (f, t3) <- parseTokens t2
  t4 <- consumeParentesis ")" t3
  Right (If c t f, t4)

-- Parsea un let con nombre, valor y cuerpo.
parseLet :: [String] -> Either EvalError (Expr, [String])
parseLet [] = Left (TypeMismatch "let incompleto")
parseLet (name : t1) = do
  (e, t2) <- parseTokens t1
  (body, t3) <- parseTokens t2
  t4 <- consumeParentesis ")" t3
  Right (Let name e body, t4)

-- Parsea una llamada de funcion con cantidad variable de argumentos.
parseApp :: [String] -> Either EvalError (Expr, [String])
parseApp [] = Left (TypeMismatch "app incompleto")
parseApp (fname : rest) = do
  (args, t1) <- parseArgs rest
  Right (App fname args, t1)

-- Parsea argumentos hasta encontrar el parentesis de cierre.
parseArgs :: [String] -> Either EvalError ([Expr], [String])
parseArgs [] = Left (TypeMismatch "falta cerrar app")
parseArgs (")" : rest) = Right ([], rest)
parseArgs tokens = do
  (arg, t1) <- parseTokens tokens
  (more, t2) <- parseArgs t1
  Right (arg : more, t2)

-- Valida que el siguiente token sea el esperado.
consumeParentesis :: String -> [String] -> Either EvalError [String]
consumeParentesis _ [] = Left (TypeMismatch "fin inesperado")
consumeParentesis expected (x : xs)
  | x == expected = Right xs
  | otherwise = Left (TypeMismatch "operacion inesperada")

-- Pipeline puro: parsea y luego evalua con entornos vacios.
evalInput :: String -> Either EvalError Value
evalInput = (>>= evaluate emptyFunEnv emptyEnv) . parseExpr

-- Convierte el resultado de Either a texto para mostrar.
runPipeline :: String -> String
runPipeline = either showError showValue . evalInput

-- Efecto de entrada: usa archivo con ':file ruta' o texto directo por consola.
readSource :: String -> IO String
readSource line =
  case words line of
    ":file" : pathParts -> readFile (unwords pathParts)
    _ -> return line

-- Punto de entrada con efectos IO: lee fuente y muestra salida.
main :: IO ()
main = do
  putStrLn "Ingresa expresion o ':file ruta'"
  entry <- getLine
  source <- readSource entry
  putStrLn (runPipeline source)


-- Ejemplos para el main:
-- 77
-- true
-- (add 9 8)
-- (if (lt 3 4) 30 40)
-- (let x 9 (mul x 7))
-- (div 8 2)
-- :file entrada.txt
