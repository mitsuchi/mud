module Lib where

  import Control.Monad (void, forM_)
  import Control.Monad.Combinators.Expr
  import Data.Void
  import Data.Char
  import Data.IORef
  import Data.List (find, intercalate, isInfixOf)
  import Data.Map as Map hiding (map, foldr, take)
  import Debug.Trace
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L
  
  import DeepList
  import TypeUtil

  type Parser = Parsec Void String
  type Name = String
  type Param = String
  type Type = String
  type Env = IORef (Map Name [(DeepList Type, Expr)])
  
  data Expr
    = IntLit Integer
    | StrLit String
    | DoubleLit Double
    | Var Name
    | BinOp Op Expr Expr
    | Seq [Expr]
    | Assign Name Expr
    | FunDef Name (DeepList Type) [Param] Expr
    | FunDefAnon (DeepList Type) [Param] Expr
    | Fun (DeepList Type) [Param] Expr Env
    | Apply Expr [Expr]
    | Case [Expr] [([Expr],Expr)] (DeepList Type)
    | TypeSig (DeepList Type) Expr
    | ListLit [Expr]
    | BoolLit Bool
    | If Expr Expr Expr -- If CondEx ThenEx ElseEx
    | Neg Expr
    | TypeDef Name [(String, DeepList Type)]
    | StructType [(String, DeepList Type)]
    | StructValue (Map Name Expr)

  data Op
    = Mul
    | Div
    | Add
    | Sub
    | Eq
    | Dot
    | RArrow
    | Colon
    | OpLit String
    | Equal
    | And
    | Or
    | Ltq
    | Gtq
    | Lt
    | Gt
    deriving (Show)

  sc :: Parser ()
  sc = L.space spaceOrTab1 lineCmnt blockCmnt
    where
      spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> c == ' ' || c == '\t')
      lineCmnt  = L.skipLineComment "#"
      blockCmnt = L.skipBlockComment "/*" "*/"

  scn :: Parser ()
  scn = L.space space1 lineCmnt blockCmnt
    where
      lineCmnt  = L.skipLineComment "#"
      blockCmnt = L.skipBlockComment "/*" "*/"
      
  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc
  
  integer :: Parser Integer
  integer = lexeme L.decimal
  
  double :: Parser Double
  double = lexeme L.float

  operator :: Parser String
  operator = lexeme $ some (oneOf "+-*/><")

  rword :: String -> Parser ()
  rword w = (lexeme . try) (space >> string w *> notFollowedBy alphaNumChar)
  
  reservedWords :: [String] -- list of reserved words
  reservedWords = ["fun","if","then","else","type"]
  
  identifier :: Parser String
  identifier = (lexeme . try) (p >>= check)
    where
      p       = (:) <$> letterChar <*> many alphaNumChar
      check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x
  
  symbol :: String -> Parser String
  symbol s = (L.symbol scn s)

  symboln :: String -> Parser String
  symboln s = (L.symbol sc s)  
  
  instance Show Expr where
    show (IntLit i1) = show i1 
    show (StrLit str) = str
    show (DoubleLit f) = show f
    show (Neg e) = "-" ++ show e
    show (Var name) = name
    show (BinOp op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Seq exprs) = foldr ((++).(++ ";").show) "" exprs  
    show (Fun types params body env) = "function : " ++ (show types)
    show (FunDef name types params body) = "(Fun (" ++ name ++ ") " ++ (show body) ++ ")"
    show (FunDefAnon types params body) = "anon fun : " ++ (show types)
    show (Apply e1 e2) = "(" ++ show (e1) ++ " " ++ show (e2) ++ ")"
    show (TypeSig sig expr) = (show expr) ++ " : " ++ (show sig)
    show (ListLit exprs) = "[" ++ (intercalate "," (map show exprs)) ++ "]"
    show (BoolLit b) = show b
    show (If condEx thenEx elseEx) = "if " ++ show (condEx) ++ " then " ++ show thenEx ++ " else " ++ show elseEx
    show (Case exprs matches types) = "(Case " ++ (show matches) ++ ")"
    show (TypeDef name types) = "(TypeDef " ++ name ++ " " ++ show types ++ ")"
    show (StructType types) = "(StructType " ++ show types ++ ")"
    show (StructValue structValue) = "(StructValue " ++ show structValue ++ ")"

  instance Eq Expr where
    (IntLit i1) == (IntLit i2) = i1 == i2
    (StrLit s1) == (StrLit s2) = s1 == s2
    (ListLit l1) == (ListLit l2) = l1 == l2
    (BoolLit b1) == (BoolLit b2) = b1 == b2
    e1 == e2 = trace (show (e1,e2)) $ False


  ops :: [[Operator Parser Expr]]
  ops =
    [ 
      [ Prefix (Neg <$ symbol "-") ]
    , [ InfixR (BinOp (OpLit "++") <$ symbol "++")
      , InfixR (BinOp (OpLit "**") <$ symbol "**") ]
    , [ InfixL (BinOp Dot <$ (symbol "." *> notFollowedBy integer)) ]
    , [ InfixL (BinOp Mul <$ symbol "*")
      , InfixL (BinOp Div <$ symbol "/") ]
    , [ InfixL (BinOp Add <$ symbol "+")
      , InfixL (BinOp Sub <$ symbol "-") ]
    , [ InfixL (BinOp Ltq <$ symbol "<=")
      , InfixL (BinOp Gtq <$ symbol "=>")
      , InfixL (BinOp Lt <$ symbol "<")
      , InfixL (BinOp Gt <$ symbol ">") ]
    , [ InfixL (BinOp And <$ symbol "&&") ]
    , [ InfixL (BinOp Or <$ symbol "||") ]
    , [ InfixR (BinOp Equal <$ symbol "==") ]
    , [ InfixR (BinOp Eq <$ symbol "=") ]
    ]
  
  expr :: Parser Expr
  expr = makeExprParser term ops
  
  term :: Parser Expr
  term = try anonFun
    <|> try apply
    <|> arg
    <|> try ifExpr
    <|> try funDefCase  
    <|> try fundef
    <|> try typeDef
  
  arg :: Parser Expr
  arg = try (DoubleLit <$> double)
    <|> IntLit <$> integer
    <|> strLit
    <|> Var <$> identifier
    <|> listLit
    <|> try (parens argWithTypeSig)
    <|> parens expr
    <|> seqExpr    

  anonFun :: Parser Expr
  anonFun = do
    params <- some identifier
    symbol "->"
    body <- expr
    symbol ":"
    sig <- typeList
    return $ FunDefAnon sig params body

  exprWithTypeSig :: Parser Expr
  exprWithTypeSig = do
    expr' <- expr
    symbol ":"
    sig <- typeList
    return $ TypeSig sig expr'

  argWithTypeSig :: Parser Expr
  argWithTypeSig = do
    arg' <- arg
    symbol ":"
    sig <- typeList
    return $ TypeSig sig arg'

  parens :: Parser a -> Parser a
  parens = between (symbol "(") (symboln ")")

  strLit :: Parser Expr
  strLit = do
    char '\''
    str <- many $ noneOf "'"
    symbol "'"
    return $ StrLit str

  listLit :: Parser Expr
  listLit = do
    symbol "["
    exprs <- sepBy expr (symbol ",")
    symboln "]"
    return $ ListLit exprs

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
  
  topLevel :: Parser Expr
  topLevel = do
    sc
    many newLine
    exprs <- many exprNewLine
    return $ Seq exprs

  typeDef :: Parser Expr
  typeDef = do
    rword "type"
    name <- identifier
    symbol "="
    symbol "{"
    types <- sepBy1 memberWithType (symbol ",")
    symbol "}"
    return $ TypeDef name types

  memberWithType :: Parser (String, DeepList Type)
  memberWithType = do
    member <- identifier
    symbol ":"
    types <- typeList
    return $ (member, types)

  fundef :: Parser Expr
  fundef = do
    rword "fun"
    name <- try identifier <|> try operator
    symbol ":"
    types <- typeList
    symbol "="
    params <- some identifier
    symbol "->"
    body <- expr
    return $ FunDef name types params body
  
  funDefCase :: Parser Expr
  funDefCase = do
    rword "fun"
    name <- try identifier <|> try operator
    symbol ":"
    types <- typeList
    symbol "="
    symbol "{"
    many newLine
    matches <- some matchExpr
    symbol "}"
    return $ FunDef name types (paramList (paramNum matches)) (Case (varList (paramNum matches)) matches types) 
      where 
        paramNum matches = length (fst (head matches))
        paramList n = zipWith (++) (take n (repeat "x")) (map show (take n [1..]))
        varList n = map Var (paramList n)

  ifExpr :: Parser Expr
  ifExpr = do
    rword "if"
    condExpr <- expr
    rword "then"
    thenExpr <- expr
    rword "else"
    elseExpr <- expr
    return $ If condExpr thenExpr elseExpr
  
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
  
  eval :: Expr -> Env -> IO Expr
  eval (IntLit i) env = return $ IntLit i
  eval (StrLit s) env = return $ StrLit s
  eval (DoubleLit f) env = return $ DoubleLit f
  eval (ListLit es) env = do
    es' <- mapM (\e -> eval e env) es
    return $ ListLit es'
  eval (Var "True") env = return $ BoolLit True
  eval (Var "False") env = return $ BoolLit False
  eval (Var name) env = do
    var <- lookupVarLoose name env
    env' <- readIORef env
    case var of
      Nothing -> error ("'" ++ name ++ "' not found, env = " ++ (show env'))
      Just x -> return x
  eval (Neg expr) env = eval (BinOp Sub (IntLit 0) expr) env
  eval (BinOp Add (IntLit i1) (IntLit i2)) env = return $ IntLit (i1+i2)
  eval (BinOp Add (DoubleLit f1) (DoubleLit f2)) env = return $ DoubleLit (f1+f2)
  eval (BinOp Add (StrLit i1) (StrLit i2)) env = return $ StrLit (i1++i2)
  eval (BinOp Add (ListLit l1) (ListLit l2)) env = return $ ListLit (l1++l2)
  eval (BinOp Add (BoolLit b1) (BoolLit b2)) env = return $ BoolLit (b1 || b2)
  eval (BinOp Add (IntLit i1) (DoubleLit f2)) env = return $ DoubleLit (fromIntegral i1 + f2)
  eval (BinOp Add (DoubleLit f1) (IntLit i2)) env = return $ DoubleLit (f1 + fromIntegral i2)
  eval (BinOp Sub (IntLit i1) (IntLit i2)) env = return $ IntLit (i1-i2)
  eval (BinOp Sub (DoubleLit f1) (DoubleLit f2)) env = return $ DoubleLit (f1-f2)
  eval (BinOp Sub (IntLit i1) (DoubleLit f2)) env = return $ DoubleLit (fromIntegral i1 - f2)
  eval (BinOp Sub (DoubleLit f1) (IntLit i2)) env = return $ DoubleLit (f1- fromIntegral i2)
  eval (BinOp Mul (IntLit i1) (IntLit i2)) env = return $ IntLit (i1*i2)
  eval (BinOp Mul (DoubleLit f1) (DoubleLit f2)) env = return $ DoubleLit (f1*f2)
  eval (BinOp Mul (StrLit s) (IntLit i)) env = return (StrLit $ (concatMap (\i -> s) [1..i]))
  eval (BinOp Mul (BoolLit b1) (BoolLit b2)) env = return $ BoolLit (b1 && b2)  
  eval (BinOp Mul (IntLit i1) (DoubleLit f2)) env = return $ DoubleLit (fromIntegral i1 * f2)
  eval (BinOp Mul (DoubleLit f1) (IntLit i2)) env = return $ DoubleLit (f1* fromIntegral i2)
  eval (BinOp Div (IntLit i1) (IntLit i2)) env = return $ IntLit (i1 `div` i2)
  eval (BinOp Div (DoubleLit f1) (DoubleLit f2)) env = return $ DoubleLit (f1/f2)
  eval (BinOp Div (IntLit i1) (DoubleLit f2)) env = return $ DoubleLit (fromIntegral i1 / f2)
  eval (BinOp Div (DoubleLit f1) (IntLit i2)) env = return $ DoubleLit (f1/ fromIntegral i2)
  eval (BinOp Eq (Var v) e) env = do
    e' <- eval e env
    eval (Assign v e') env  
  eval (BinOp Dot e1 (Var v)) env = eval (Apply (Var v) [e1]) env
  eval (BinOp Dot e1 (Apply expr args)) env = eval (Apply expr (e1 : args)) env
  eval (BinOp (OpLit lit) e1 e2) env = eval (Apply (Var lit) [e1, e2]) env  
  eval (BinOp And (BoolLit b1) (BoolLit b2)) env = return $ BoolLit (b1 && b2)  
  eval (BinOp Or (BoolLit b1) (BoolLit b2)) env = return $ BoolLit (b1 || b2)  
  eval (BinOp Equal (IntLit i1) (IntLit i2)) env = return $ BoolLit (i1 == i2)  
  eval (BinOp Equal (StrLit s1) (StrLit s2)) env = return $ BoolLit (s1 == s2)
  eval (BinOp Equal (BoolLit b1) (BoolLit b2)) env = return $ BoolLit (b1 == b2)  
  eval (BinOp Equal (ListLit l1) (ListLit l2)) env = return $ BoolLit (l1 == l2)  
  eval (BinOp Lt (IntLit i1) (IntLit i2)) env = return $ BoolLit (i1 < i2)
  eval (BinOp Ltq (IntLit i1) (IntLit i2)) env = return $ BoolLit (i1 <= i2)
  eval (BinOp Gt (IntLit i1) (IntLit i2)) env = return $ BoolLit (i1 > i2)
  eval (BinOp Gtq (IntLit i1) (IntLit i2)) env = return $ BoolLit (i1 >= i2)
  eval (BinOp Lt (DoubleLit i1) (DoubleLit i2)) env = return $ BoolLit (i1 < i2)
  eval (BinOp Ltq (DoubleLit i1) (DoubleLit i2)) env = return $ BoolLit (i1 <= i2)
  eval (BinOp Gt (DoubleLit i1) (DoubleLit i2)) env = return $ BoolLit (i1 > i2)
  eval (BinOp Gtq (DoubleLit i1) (DoubleLit i2)) env = return $ BoolLit (i1 >= i2)
  eval (BinOp Lt (IntLit i1) (DoubleLit i2)) env = return $ BoolLit (fromIntegral i1 < i2)
  eval (BinOp Lt (DoubleLit i1) (IntLit i2)) env = return $ BoolLit (i1 < fromIntegral i2)
  eval (BinOp Ltq (IntLit i1) (DoubleLit i2)) env = return $ BoolLit (fromIntegral i1 <= i2)
  eval (BinOp Ltq (DoubleLit i1) (IntLit i2)) env = return $ BoolLit (i1 <= fromIntegral i2)
  eval (BinOp Gt (IntLit i1) (DoubleLit i2)) env = return $ BoolLit (fromIntegral i1 > i2)
  eval (BinOp Gt (DoubleLit i1) (IntLit i2)) env = return $ BoolLit ( i1 > fromIntegral i2)
  eval (BinOp Gtq (IntLit i1) (DoubleLit i2)) env = return $ BoolLit (fromIntegral i1 >= i2)
  eval (BinOp Gtq (DoubleLit i1) (IntLit i2)) env = return $ BoolLit (i1 >= fromIntegral i2)
  eval (BinOp Add e1@(StructValue s1) e2) env = eval (Apply (Var "+") [e1, e2]) env
  eval (BinOp Add e1 e2@(StructValue s1)) env = eval (Apply (Var "+") [e1, e2]) env
  eval (BinOp Sub e1@(StructValue s1) e2) env = eval (Apply (Var "-") [e1, e2]) env
  eval (BinOp Sub e1 e2@(StructValue s2)) env = eval (Apply (Var "-") [e1, e2]) env
  eval (BinOp Mul e1@(StructValue s1) e2) env = eval (Apply (Var "*") [e1, e2]) env
  eval (BinOp Mul e1 e2@(StructValue s2)) env = eval (Apply (Var "*") [e1, e2]) env
  eval (BinOp Div e1@(StructValue s1) e2) env = eval (Apply (Var "/") [e1, e2]) env
  eval (BinOp Div e1 e2@(StructValue s2)) env = eval (Apply (Var "/") [e1, e2]) env  
  eval (BinOp op e1 e2) env = do
    e1' <- eval e1 env
    e2' <- eval e2 env
    eval (BinOp op e1' e2') env
  eval (Seq (e:[])) env = eval e env
  eval (Seq (e:es)) env = do
    eval e env
    eval (Seq es) env
  eval (Assign name fun@(Fun types params expr outerEnv)) env = do
    res <- insertFun name types fun env
    case res of
      Left message -> error message
      Right env -> return expr
  eval (Assign name expr) env = do
    res <- insertVar name expr env
    case res of
      Left message -> error message
      Right env -> return expr
  eval (FunDef name types params body) env = do
    eval (Assign name (Fun types params body env)) env  
  eval (FunDefAnon types params body) env = do
    return $ Fun types params body env
  eval (Apply (Fun types params body outerEnv) args) env = do
    varMap <- readIORef outerEnv
    env' <- newEnv params args varMap
    eval body env'
  eval (Apply (Var name) args) env = do
    args' <- mapM (\arg -> eval arg env) args
    fun' <- lookupFun name (Plain (map typeOf' args')) env
    case fun' of
      Just fun -> eval (Apply fun args') env
      Nothing -> call name args' env
  eval (Apply expr args) env = do
    expr' <- eval expr env
    args' <- mapM (\arg -> eval arg env) args
    eval (Apply expr' args') env
  eval (Case es matchPairs types) env = do
    es' <- mapM (\e -> eval e env ) es
    case find (\pair -> matchCond es' (fst pair)) matchPairs of
      Just pair -> let (params, args) = paramsAndArgs (fst pair) es'
                   in eval (Apply (Fun types params (snd pair) env) args) env
      Nothing   -> error "condition no match"
  eval expr@(TypeSig sig (Var name)) env = do
    fun' <- lookupFun name sig env
    case fun' of
      Just fun -> return fun
      Nothing -> do
        env' <- readIORef env
        error (name ++ " not found , env = " ++ (show env'))
  eval (TypeSig sig expr) env = eval expr env
  eval (If condExpr thenExpr elseExpr) env = do
    cond' <- eval condExpr env
    case cond' of
      BoolLit True -> eval thenExpr env
      BoolLit False -> eval elseExpr env
  eval (TypeDef name typeDef) env = do
    forM_ typeDef $ \(member, (Plain [typeList])) -> do
      eval (FunDef member (Plain [Elem name, typeList]) ["x"] (Apply (Var "lookupStruct") [Var "x", StrLit member])) env
    eval (FunDef name types params (Apply (Var "makeStruct") (StrLit name : map StrLit params))) env    
    where types = typeDefToTypes typeDef
          params = map fst typeDef
  eval value@(StructValue structValue) env = return value

  -- [("r",Plain [Elem "Int"]),("i",Plain [Elem "Int"])]
  -- を以下に変換する
  -- Plain [Elem "Int", Elem "Int"]
  typeDefToTypes :: [(Name, DeepList Type)] -> DeepList Type
  typeDefToTypes es = Plain (foldMap ((++) . (\(Plain x) -> x) . snd) es [])

  call :: Name -> [Expr] -> Env -> IO Expr
  call "head" [ListLit (e:es)] env = return e
  call "tail" [ListLit (e:es)] env = return $ ListLit es
  call "puts" [e] env = do
    print e
    return e
  call "makeStruct" (name:args) env = do
    makeStruct args (Map.fromList [("type", name)]) env
  call "lookupStruct" [StructValue structValue, StrLit member] env = do
    case Map.lookup member structValue of
      Just expr -> return expr
      Nothing -> error ("can't find struct member '" ++ member ++ "'")
  call name args env = do
    env' <- readIORef env
    error (name ++ " not found, args = " ++ (show args) ++ ", env = " ++ (show env'))

  makeStruct :: [Expr] -> Map Name Expr -> Env -> IO Expr
  makeStruct [] m env = return $ StructValue m
  makeStruct (StrLit name : es) m env = do
    var <- lookupVar name env
    case var of 
      Just expr -> makeStruct es (Map.insert name expr m) env
      Nothing -> error "can't find struct member"

  typeOf' :: Expr -> DeepList String
  typeOf' (IntLit i) = Elem "Int"
  typeOf' (StrLit s) = Elem "String"
  typeOf' (BoolLit b) = Elem "Bool"
  typeOf' (DoubleLit b) = Elem "Double"
  typeOf' (TypeSig sig _) = sig
  typeOf' (Fun sig _ _ _) = sig
  typeOf' (ListLit (e:es)) = Plain [Elem "List", typeOf' e]
  typeOf' (ListLit []) = Plain [Elem "List", Elem "a"]
  typeOf' (StructValue s) = case Map.lookup "type" s of
    Just (StrLit str) -> Elem str
    Nothing           -> error "type not defined in struct value"

  newEnv :: [Name] -> [Expr] -> (Map Name [(DeepList Type, Expr)]) -> IO Env
  newEnv params args outerEnv = do
    env <- newIORef outerEnv
    mapM_ (\p -> insertAny p env) (zip params args)
    return env

  insertAny :: (Name, Expr) -> Env -> IO (Either String Env)
  insertAny (name, expr) env = case expr of
    (Fun types _ _ _) -> insertFun name types expr env
    otherwise         -> insertVarForce name expr env

  fromVars :: [Expr] -> [String]
  fromVars (Var v:[]) = [v]
  fromVars (Var v:es) = v : fromVars es
  fromVars (e:es) = fromVars es
  
  paramsAndArgs :: [Expr] -> [Expr] -> ([String], [Expr])
  paramsAndArgs [] [] = ([],[])
  paramsAndArgs (Var v:e1s) (e:e2s) = let rests = paramsAndArgs e1s e2s
                                      in (v : (fst rests), e : (snd rests))
  paramsAndArgs (ListLit [Var h,Var t]:e1s) (ListLit (e2:e2'):e2s) = 
    let rests = paramsAndArgs e1s e2s
    in (h : t : (fst rests), e2 : (ListLit e2') : (snd rests))
  paramsAndArgs (e1:e1s) (e2:e2s) = paramsAndArgs e1s e2s
  
  matchCond :: [Expr] -> [Expr] -> Bool
  matchCond (IntLit i:e1s) (IntLit j:e2s) = i == j && matchCond e1s e2s
  matchCond ((IntLit i):e1s) ((Var v):e2s) = matchCond e1s e2s
  matchCond (DoubleLit i:e1s) (DoubleLit j:e2s) = i == j && matchCond e1s e2s
  matchCond ((DoubleLit i):e1s) ((Var v):e2s) = matchCond e1s e2s  
  matchCond ((ListLit l1):e1s) ((ListLit [Var h, Var t]):e2s) = matchCond e1s e2s
  matchCond ((ListLit l1):e1s) ((ListLit l2):e2s) = l1 == l2 && matchCond e1s e2s
  matchCond ((ListLit l):e1s) ((Var v):e2s) = matchCond e1s e2s
  matchCond ((Fun _ _ _ _):e1s) ((Var v):e2s) = matchCond e1s e2s
  matchCond [] [] = True
  matchCond e1 e2 = trace ("matchCond: " ++ show (e1,e2)) $ False
  
  parseExpr :: String -> Expr
  parseExpr program = case parse expr "<stdin>" program of
    Right expr -> expr
    Left bundle -> error (errorBundlePretty bundle)  

  pa :: String -> Either (ParseErrorBundle String Void) Expr
  pa program = parse topLevel "<stdin>" program
  
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
  
  runEval :: Expr -> IO ()
  runEval expr = do
    env <- newIORef Map.empty
    expr' <- eval expr env
    print expr'  

  typeList :: Parser (DeepList String)
  typeList = do
    term1 <- typeTerm
    terms <- many $ (symbol "->") *> typeTerm
    return $ Plain (term1 : terms)

  typeTerm :: Parser (DeepList String)
  typeTerm = try listTerm
    <|> (Elem <$> identifier)
    <|> parens typeList

  listTerm :: Parser (DeepList String)
  listTerm = do
    symbol "["
    term <- identifier
    symbol "]"
    return $ Plain [ Elem "List", Elem term ]

  makeMap :: [String] -> Map String Int
  makeMap list = makeMap' list 0 Map.empty

  makeMap' :: [String] -> Int -> Map String Int -> Map String Int
  makeMap' [] num table = table
  makeMap' (e:es) num table = if isUpper(e !! 0) then (makeMap' es num table) else 
    case Map.lookup e table of
      Nothing -> makeMap' es (num+1) (Map.insert e num table)
      Just i  -> makeMap' es num table
  
  generalizeTypeSig :: DeepList String -> DeepList String
  generalizeTypeSig list = gnrlize' list (makeMap (dFlatten list))

  gnrlize' :: DeepList String -> Map String Int -> DeepList String
  gnrlize' (Elem e) table = case Map.lookup e table of
    Nothing -> Elem e
    Just i -> Elem ("t" ++ show i)
  gnrlize' (Plain []) table = Plain []
  gnrlize' (Plain (e:es)) table = 
    let (Plain rest') = (gnrlize' (Plain es) table)
    in Plain ((gnrlize' e table) : rest')

  -- 変数を環境に登録する
  -- 同名の変数も関数もない場合のみ登録できる
  insertVar :: Name -> Expr -> Env -> IO (Either String Env)
  insertVar name expr env = do
    e <- anyExists name env
    if not e
    then insertVarForce name expr env
    else return $ Left (name ++ " already exists")

  insertVarForce :: Name -> Expr -> Env -> IO (Either String Env)
  insertVarForce name expr env = do
    env' <- readIORef env
    writeIORef env (Map.insert name [(Elem "_", expr)] env')
    return $ Right env

  varExists :: Name -> Env -> IO Bool
  varExists name env = do
    exists <- lookupVar name env
    case exists of
      Nothing -> return False
      otherwise -> return True

  lookupVar :: Name -> Env -> IO (Maybe Expr)
  lookupVar name env = lookupVar' name env False

  lookupVarLoose :: Name -> Env -> IO (Maybe Expr)
  lookupVarLoose name env = lookupVar' name env True

  lookupVar' :: Name -> Env -> Bool -> IO (Maybe Expr)
  lookupVar' name env loose = do
    env' <- readIORef env
    case Map.lookup name env' of
      Nothing -> return Nothing
      Just vars -> case vars of
        (Elem "_", expr):[] -> return $ Just expr
        -- 変数として名前がなくても関数として1つだけ名前があるならそれを参照する
        (Plain _, expr):[] -> return $ if loose then Just expr else Nothing

  anyExists :: Name -> Env -> IO Bool
  anyExists name env = do
    env' <- readIORef env
    case Map.lookup name env' of
      Nothing -> return False
      Just vars -> return True

  showEnv :: Env -> IO ()
  showEnv env = do
    env' <- readIORef env
    print env'

  showResult :: Either String Env -> IO ()
  showResult res = case res of
    Left message -> print message
    Right env -> showEnv env

  -- 関数を環境に登録する
  -- 同名の変数も、同型の関数もない場合のみ登録できる
  -- ただし、同名の関数がある場合はその定義の末尾に追加する
  insertFun :: Name -> DeepList Type -> Expr -> Env -> IO (Either String Env)      
  insertFun name types expr env = do
    ve <- varExists name env
    fe <- funExists name types env
    if not ve && not fe
    then do
      insertFun' name (generalizeTypeSig types) expr env
      return $ Right env
    else
      return $ Left (name ++ " already exists"  )

  insertFun' :: Name -> DeepList Type -> Expr -> Env -> IO Env
  insertFun' name types expr env = do
    env' <- readIORef env
    funs' <- case Map.lookup name env' of
      Nothing -> return []
      Just funs -> return funs
    writeIORef env (Map.insert name (funs' ++ [(generalizeTypeSig types, expr)]) env')
    return env

  funExists :: Name -> DeepList Type -> Env -> IO Bool
  funExists name types env = do
    fun <- lookupFun name types env
    case fun of
      Nothing -> return $ False
      Just expr -> return $ True

  lookupFun :: Name -> DeepList Type -> Env -> IO (Maybe Expr)
  lookupFun name types env = do
    env' <- readIORef env
    case Map.lookup name env' of
      Nothing -> return Nothing
      Just funs -> case funs of
        (Elem "_", expr):[] -> return Nothing
        otherwise -> case firstMatch (generalizeTypeSig types) funs of
          Nothing -> return Nothing
          Just fun -> return $ Just fun

  firstMatch :: DeepList Type -> [(DeepList Type, Expr)] -> Maybe Expr
  firstMatch types [] = Nothing
  firstMatch types ((types', expr):es) = case findTypeEnv types' types Map.empty of
    Nothing -> firstMatch types es
    Just env -> Just expr    