module Main (main) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Char
import Data.IORef
import Data.List (find)
import Data.Map as Map hiding (map, foldr, take)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space spaceOrTab1 lineCmnt blockCmnt
  where
    spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> c == ' ' || c == '\t')
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String] -- list of reserved words
reservedWords = ["fun"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

symbol :: String -> Parser String
symbol = L.symbol sc

type Name = String
type Param = String

data Expr
  = IntLit Integer
  | Var Name
  | BinOp Op Expr Expr
  | Seq [Expr]
  | Assign Name Expr
  | FunDef Name [Param] Expr
  | Fun [Param] Expr Env
  | Apply Expr [Expr]
  | Case [Expr] [([Expr],Expr)]

instance Show Expr where
  show (IntLit i1) = show i1  
  show (Var name) = name
  show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (Seq exprs) = foldr ((++).(++ ";").show) "" exprs  
  show (Fun param body env) = "function"
  show (FunDef name param body) = name
  show (Apply e1 e2) = "application"

data Op
  = Mul
  | Div
  | Add
  | Sub
  | Eq
  | Dot
  deriving (Show)

ops :: [[Operator Parser Expr]]
ops =
  [ 
    [ InfixL (BinOp Dot <$ symbol ".") ]
  , [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/") ]
  , [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-") ]
  , [ InfixR (BinOp Eq <$ symbol "=") ]
  ]

expr :: Parser Expr
expr = makeExprParser term ops

term :: Parser Expr
term = try apply
  <|> arg

arg :: Parser Expr
arg = IntLit <$> integer
  <|> Var <$> identifier
  <|> parens expr
  <|> seqExpr
  <|> try funDefCase  
  <|> fundef

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

seqExpr :: Parser Expr
seqExpr = do
  symbol "{"
  many newLine
  exprs <- many exprNewLine
  symbol "}"
  return $ Seq exprs

exprNewLine :: Parser Expr
exprNewLine = do
  e <- expr
  many newLine
  return e

newLine :: Parser String
newLine = symbol "\n" <|> symbol ";"

fundef :: Parser Expr
fundef = do
  rword "fun"
  name <- identifier
  symbol "="
  params <- some identifier
  symbol "->"
  body <- expr
  return $ FunDef name params body

funDefCase :: Parser Expr
funDefCase = do
  rword "fun"
  name <- identifier
  symbol "="
  symbol "{"
  many newLine
  matches <- some matchExpr
  symbol "}"
  return $ FunDef name (paramList (paramNum matches)) (Case (varList (paramNum matches)) matches) 
    where 
      paramNum matches = length (fst (head matches))
      paramList n = zipWith (++) (take n (repeat "x")) (map show (take n [1..]))
      varList n = map Var (paramList n)

matchExpr :: Parser ([Expr], Expr)
matchExpr = do
  conds <- some arg
  symbol "->"
  body <- expr
  many newLine
  return (conds, body)

apply :: Parser Expr
apply = do
  caller <- parens expr <|> (Var <$> identifier)
  args <- some arg
  return $ Apply caller args

type Dict = Map String Expr
type Env = IORef (Map String Expr)

eval :: Expr -> Env -> IO Expr
eval (IntLit i) env = return $ IntLit i
eval (Var name) env = do
  varMap <- readIORef env
  case Map.lookup name varMap of
    Just x -> return x
    Nothing -> error ("'" ++ name ++ "' not found")
eval (BinOp Add (IntLit i1) (IntLit i2)) env = return $ IntLit (i1+i2)
eval (BinOp Sub (IntLit i1) (IntLit i2)) env = return $ IntLit (i1-i2)
eval (BinOp Mul (IntLit i1) (IntLit i2)) env = return $ IntLit (i1*i2)
eval (BinOp Div (IntLit i1) (IntLit i2)) env = return $ IntLit (i1 `div` i2)
eval (BinOp Eq (Var v) e) env = do
  e' <- eval e env
  eval (Assign v e') env  
eval (BinOp Dot e1 (Var v)) env = eval (Apply (Var v) [e1]) env
eval (BinOp Dot e1 (Apply expr args)) env = eval (Apply expr (e1 : args)) env
eval (BinOp op e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  eval (BinOp op e1' e2') env
eval (Seq (e:[])) env = eval e env
eval (Seq (e:es)) env = do
  eval e env
  eval (Seq es) env
eval (Assign name expr) env = do
  varMap <- readIORef env
  writeIORef env (Map.insert name expr varMap)
  return expr
eval (FunDef name param body) env = do
  eval (Assign name (Fun param body env)) env  
eval (Apply (Fun params body outerEnv) args) env = do
  varMap <- readIORef outerEnv
  env' <- newIORef (newEnv params args varMap)
  eval body env'
eval (Apply expr args) env = do
  expr' <- eval expr env
  args' <- mapM (\arg -> eval arg env) args
  eval (Apply expr' args') env
eval (Case es matchPairs) env = do
  es' <- mapM (\e -> eval e env ) es
  case find (\pair -> matchCond es' (fst pair)) matchPairs of
    Just pair -> let (params, args) = paramsAndArgs (fst pair) es'
                 in eval (Apply (Fun params (snd pair) env) args) env
    Nothing   -> error "condition no match"  

newEnv :: [Name] -> [Expr] -> Dict -> Dict
newEnv params args outerEnv = foldMap (\p -> Map.insert (fst p) (snd p) outerEnv) (zip params args)

fromVars :: [Expr] -> [String]
fromVars (Var v:[]) = [v]
fromVars (Var v:es) = v : fromVars es
fromVars (e:es) = fromVars es

paramsAndArgs :: [Expr] -> [Expr] -> ([String], [Expr])
paramsAndArgs [] [] = ([],[])
paramsAndArgs (Var v:e1s) (e:e2s) = let rests = paramsAndArgs e1s e2s
                                    in (v : (fst rests), e : (snd rests))
paramsAndArgs (e1:e1s) (e2:e2s) = paramsAndArgs e1s e2s

matchCond :: [Expr] -> [Expr] -> Bool
matchCond (IntLit i:e1s) (IntLit j:e2s) = i == j && matchCond e1s e2s
matchCond ((IntLit i):e1s) ((Var v):e2s) = matchCond e1s e2s
matchCond [] [] = True

pa :: String -> Either (ParseErrorBundle String Void) Expr
pa program = parse expr "<stdin>" program

ev :: String -> IO ()
ev program = case pa program of
  Right expr -> runEval expr
  Left bundle -> putStr (errorBundlePretty bundle)

evf :: String -> IO ()
evf file = do 
  program <- readFile file
  ev program

paf :: String -> IO ()
paf file = do 
  program <- readFile file
  print $ pa program

main :: IO ()
main = do
  input <- getContents
  parseTest expr input

runEval :: Expr -> IO ()
runEval expr = do
  env <- newIORef Map.empty
  expr' <- eval expr env
  print expr'  