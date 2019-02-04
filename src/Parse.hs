module Parse where

  import Control.Monad (void)
  import Control.Monad.Combinators.Expr
  import Debug.Trace
  import Data.Void
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L

  import DeepList
  import Expr
  import Tuple

  type Parser = Parsec Void String

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
  identifier = (lexeme . try) (identifier' >>= check)
    where
      check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x
  
  identifier' :: Parser String                  
  identifier' = do
    firstLetter <- letterChar
    middleLetters <- many ( oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) )
    lastLetters <- many (oneOf "!?_'")
    return $ firstLetter : (middleLetters ++ lastLetters)


  symbol :: String -> Parser String
  symbol s = (L.symbol scn s)

  symboln :: String -> Parser String
  symboln s = (L.symbol sc s)  

  ops :: [[Operator Parser Expr]]
  ops =
    [ 
      [ Prefix (Neg <$ symbol "-") ]
    , [ InfixR (BinOp (OpLit "++") <$> (symbol "++" *> getCode))
      , InfixR (BinOp (OpLit "**") <$> (symbol "**" *> getCode)) ]
    , [ InfixL (BinOp Dot <$> (symbol "." *> getCode <* notFollowedBy integer)) ]
    , [ InfixL (BinOp (OpLit "*") <$> (symbol "*" *> getCode))
      , InfixL (BinOp (OpLit "/") <$> (symbol "/" *> getCode)) ]
    , [ InfixL (BinOp (OpLit "+") <$> (symbol "+" *> getCode))
      , InfixL (BinOp (OpLit "-") <$> (symbol "-" *> getCode)) ]
    , [ InfixL (BinOp (OpLit "<=") <$> (symbol "<=" *> getCode))
      , InfixL (BinOp (OpLit "=>") <$> (symbol "=>" *> getCode))
      , InfixL (BinOp (OpLit "<") <$> (symbol "<" *> getCode))
      , InfixL (BinOp (OpLit ">") <$> (symbol ">" *> getCode)) ]
    , [ InfixR (BinOp (OpLit "==") <$> (symbol "==" *> getCode)) ]
    , [ InfixL (BinOp (OpLit "&&") <$> (symbol "&&" *> getCode)) ]
    , [ InfixL (BinOp (OpLit "||") <$> (symbol "||" *> getCode)) ]
    , [ InfixR (BinOp Eq <$> (symbol "=" *> getCode)) ]
    ]
  
  expr :: Parser Expr
  expr = makeExprParser term ops
  
  term :: Parser Expr
  term = try anonFun
    <|> try apply
    <|> arg
    <|> try ifExpr
    <|> try funDefCase  
    <|> fundef
    <|> typeDef
  
  arg :: Parser Expr
  arg = try (DoubleLit <$> double)
    <|> IntLit <$> integer
    <|> strLit
    <|> Var <$> identifier <*> getCode
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
    beginChar <- char '"' <|> char '\''
    str <- many $ noneOf (beginChar : "")
    symboln (beginChar : "")
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
    exprs <- some exprNewLine
    return $ Seq exprs

  typeDef :: Parser Expr
  typeDef = do
    rword "type"
    name <- identifier
    symbol "="
    symbol "{"
    types <- sepBy1 memberWithType (symbol ",")
    many newLine
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
    name <- try identifier <|> operator
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
        paramNum matches = length (fst3 (head matches))
        paramList n = zipWith (++) (take n (repeat "x")) (map show (take n [1..]))
        varList n = map (\v -> Var v (Code { lineOfCode = 1 })) (paramList n)

  ifExpr :: Parser Expr
  ifExpr = do
    rword "if"
    condExpr <- expr
    rword "then"
    thenExpr <- expr
    rword "else"
    elseExpr <- expr
    return $ If condExpr thenExpr elseExpr
  
  matchExpr :: Parser ([Expr], Expr, Maybe Expr)
  matchExpr = do
    conds <- some arg
    guard <- optional ( symbol "|" *> expr <* symbol "|")
    symbol "->"
    body <- expr
    many newLine
    return (conds, body, guard)
  
  apply :: Parser Expr
  apply = do
    caller <- parens expr <|> (Var <$> identifier <*> getCode)
    args <- some arg
    return $ Apply caller args

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
  
  getCode :: Parser Code
  getCode = do
    pos <- getSourcePos
    return $ Code { lineOfCode = unPos (sourceLine pos) }