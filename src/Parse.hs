-- 字句解析と構文解析
module Parse where

  import Control.Monad (void)
  import Control.Monad.Combinators.Expr
  import Debug.Trace
  import Data.Void
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L

  import RecList
  import Expr

  type Parser = Parsec Void String

  -- space consumer 。空白やコメントをスキップする。改行はスキップしない。
  sc :: Parser ()
  sc = L.space spaceOrTab1 lineCmnt blockCmnt
    where
      spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> c == ' ' || c == '\t')
      lineCmnt  = L.skipLineComment "#"
      blockCmnt = L.skipBlockComment "/*" "*/"

  -- 改行を含む空白やコメントをスキップする
  scn :: Parser ()
  scn = L.space space1 lineCmnt blockCmnt
    where
      lineCmnt  = L.skipLineComment "#"
      blockCmnt = L.skipBlockComment "/*" "*/"
      
  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc
  
  -- 整数を読む
  integer :: Parser Integer
  integer = lexeme L.decimal
  
  -- 浮動小数点数を読む
  double :: Parser Double
  double = lexeme L.float

  -- 演算子を読む
  operator :: Parser String
  operator = lexeme $ some (oneOf "+-*/><")

  -- 予約語を読む
  rword :: String -> Parser ()
  rword w = (lexeme . try) (space >> string w *> notFollowedBy alphaNumChar)
  
  -- 予約語のリスト
  reservedWords :: [String] -- list of reserved words
  reservedWords = ["fun","if","then","else","type"]
  
  -- 識別子を読む
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

  -- 与えられた文字列を読む。後ろの空白（改行を含む）をスキップする。
  symbol :: String -> Parser String
  symbol s = (L.symbol scn s)

  -- 与えられた文字列を読む。後ろの空白（改行を含まない）をスキップする。
  symboln :: String -> Parser String
  symboln s = (L.symbol sc s)  

  -- 演算子とその処理。リストの先頭のほうが優先順位が高い。
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
  
  -- 式を読む
  expr :: Parser Expr
  expr = makeExprParser term ops
  
  -- 項を読む。項は演算子の引数になるもの。
  term :: Parser Expr
  term = try anonFun
    <|> try apply
    <|> arg
    <|> try ifExpr
    <|> try funDefCase  
    <|> fundef
    <|> typeDef
  
  -- 関数の引数になりうるものを読む
  arg :: Parser Expr
  arg = try (DoubleLit <$> double)
    <|> IntLit <$> integer
    <|> strLit
    <|> Var <$> identifier <*> getCode
    <|> listLit
    <|> try (parens argWithTypeSig)
    <|> parens expr
    <|> seqExpr    

  -- 匿名関数を読む
  anonFun :: Parser Expr
  anonFun = do
    params <- some identifier
    symbol "->"
    code <- getCode
    body <- expr
    sig' <- optional (symbol ":" *> typeList)
    sig <- case sig' of
      Just list -> return list
      -- 型を省略した場合はもっとも一般的な型にしちゃう
      Nothing -> return $ makeGeneralType (length params)
    return $ FunDefAnon sig params body code

  -- 型を一般的な形にする。例：a -> b であれば t0 -> t1
  makeGeneralType :: Int -> RecList String
  makeGeneralType n = Elems (map (\x -> Elem ("t" ++ show x)) [0..n])

  -- 型注釈つきの式を読む
  exprWithTypeSig :: Parser Expr
  exprWithTypeSig = do
    expr' <- expr
    symbol ":"
    sig <- typeList
    return $ TypeSig sig expr'

  -- 型注釈つきの項を読む
  argWithTypeSig :: Parser Expr
  argWithTypeSig = do
    arg' <- arg
    symbol ":"
    sig <- typeList
    return $ TypeSig sig arg'

  -- カッコで挟まれる表現を読む
  parens :: Parser a -> Parser a
  parens = between (symbol "(") (symboln ")")

  -- 文字列のリテラルを読む
  strLit :: Parser Expr
  strLit = do
    beginChar <- char '"' <|> char '\''
    str <- many $ noneOf (beginChar : "")
    symboln (beginChar : "")
    return $ StrLit str

  -- リストのリテラルを読む
  listLit :: Parser Expr
  listLit = do
    symbol "["
    exprs <- sepBy expr (symbol "," <|> symbol ";")
    symboln "]"
    code <- getCode
    return $ ListLit exprs code

  -- 複式（改行で区切られて連続する式）を読む
  seqExpr :: Parser Expr
  seqExpr = do
    symbol "{"
    many newLine
    exprs <- many exprNewLine
    symbol "}"
    return $ Seq exprs
  
  -- 式を読む。後ろの改行の連続をスキップする
  exprNewLine :: Parser Expr
  exprNewLine = do
    e <- expr
    many newLine
    return e
  
  -- 改行を読む。; も改行扱いとする。
  newLine :: Parser String
  newLine = symbol "\n" <|> symbol ";"
  
  -- プログラムのトップレベルを読む
  topLevel :: Parser Expr
  topLevel = do
    sc
    many newLine
    exprs <- some exprNewLine
    return $ Seq exprs

  -- 型定義を読む
  typeDef :: Parser Expr
  typeDef = do
    rword "type"
    name <- Var <$> identifier <*> getCode
    symbol "="
    symbol "{"
    types <- sepBy1 memberWithType (symbol ",")
    many newLine
    symbol "}"
    return $ TypeDef name types

  -- 型定義中の、構造体のメンバーとその型を読む
  memberWithType :: Parser (String, RecList Type)
  memberWithType = do
    member <- identifier
    symbol ":"
    types <- typeList
    return $ (member, types)

  -- 関数定義を読む
  fundef :: Parser Expr
  fundef = do
    rword "fun"
    nameExpr <- try (Var <$> identifier <*> getCode) <|> (Var <$> operator <*> getCode)
    types' <- optional (symbol ":" *> typeList)
    symbol "="
    params <- some identifier
    symbol "->"
    body <- expr
    types <- case types' of
      Just list -> return list
      -- 型を省略した場合はもっとも一般的な型にしちゃう
      Nothing -> return $ makeGeneralType (length params)
    return $ FunDef nameExpr types params body
  
  -- パターンマッチを含む関数定義を読む
  funDefCase :: Parser Expr
  funDefCase = do
    rword "fun"
    nameExpr <- try (Var <$> identifier <*> getCode) <|> try (Var <$> operator <*> getCode) 
    types' <- optional (symbol ":" *> typeList)
    symbol "="
    symbol "{"
    many newLine
    matches <- some matchExpr
    symbol "}"
    types <- case types' of
      Just list -> return list
      -- 型を省略した場合はもっとも一般的な型にしちゃう
      Nothing -> return $ makeGeneralType (paramNum matches) 
    return $ FunDef nameExpr types (paramList (paramNum matches)) (Case (varList (paramNum matches)) matches types)
      where 
        paramNum matches = length (fst4 (head matches))
        paramList n = zipWith (++) (take n (repeat "x")) (map show (take n [1..]))
        varList n = map (\v -> Var v (Code { lineOfCode = 1 })) (paramList n)
        fst4 (a,b,c,d) = a
        
  -- if式を読む
  ifExpr :: Parser Expr
  ifExpr = do
    rword "if"
    code <- getCode
    condExpr <- expr
    rword "then"
    thenExpr <- expr
    rword "else"
    elseExpr <- expr
    return $ If condExpr thenExpr elseExpr code
  
  -- パターンマッチ式を読む
  matchExpr :: Parser ([Expr], Expr, Maybe Expr, Code)
  matchExpr = do
    conds <- some arg
    guard <- optional ( symbol "|" *> expr <* symbol "|")
    symbol "->"
    code <- getCode    
    body <- expr
    many newLine
    return (conds, body, guard, code)
  
  -- 関数適用を読む
  apply :: Parser Expr
  apply = do
    caller <- parens expr <|> (Var <$> identifier <*> getCode)
    args <- some arg
    return $ Apply caller args

  -- 型注釈を読む
  typeList :: Parser (RecList String)
  typeList = do
    term1 <- typeTerm
    terms <- many $ (symbol "->") *> typeTerm
    return $ Elems (term1 : terms)

  -- 型を表す項を読む。Int, a, [Double], (Int->String) など。
  typeTerm :: Parser (RecList String)
  typeTerm = try listTerm
    <|> (Elem <$> identifier)
    <|> parens typeList

  -- リスト型を読む
  listTerm :: Parser (RecList String)
  listTerm = do
    symbol "["
    term <- identifier
    symbol "]"
    return $ Elems [ Elem "List", Elem term ]    
  
  -- 現在パース中のコード位置を取得する
  getCode :: Parser Code
  getCode = do
    pos <- getSourcePos
    return $ Code { lineOfCode = unPos (sourceLine pos) }