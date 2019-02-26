-- 式
module Expr where

  import Data.Char
  import Data.IORef
  import Data.List (intercalate)
  import Data.Map as Map hiding (map, foldr, take)
  import Data.Maybe
  import Debug.Trace

  import RecList
  import Env

  type Name = String
  type Param = String
  type Type = String
  type Env = GeneralEnv Expr
  
  data Code = Code { lineOfCode :: Int } deriving (Show)
  emptyCode = Code { lineOfCode = 0 }

  data Expr
    = IntLit Integer
    | StrLit String
    | DoubleLit Double
    | Var Name Code
    | BinOp Op Code Expr Expr
    | Seq [Expr]
    | Assign NameExpr Expr
    | FunDef NameExpr (RecList Type) [Param] Expr
    | FunDefAnon (RecList Type) [Param] Expr Code
    | Fun (RecList Type) [Param] Expr Env
    | Apply Expr [Expr]
    | Case [Expr] [([Expr],Expr,Maybe Expr,Code)] (RecList Type)
    | TypeSig (RecList Type) Expr
    | TypeLit (RecList Type)
    | ListLit [Expr] Code
    | BoolLit Bool
    | If Expr Expr Expr Code -- If CondEx ThenEx ElseEx
    | Neg Expr
    | TypeDef NameExpr [(String, RecList Type)]
    | StructType [(String, RecList Type)]
    | StructValue (Map Name Expr)
    | Call Name (RecList Type)

  type NameExpr = Expr

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

  instance Show Expr where
    show (IntLit i1) = show i1 
    show (StrLit str) = str
    show (DoubleLit f) = show f
    show (Neg e) = "-" ++ show e
    show (Var name _) = name
    show (BinOp op _ e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Seq exprs) = foldr ((++).(++ ";").show) "" exprs  
    show (Fun types params body env) = "function : " ++ (show types)
    show (FunDef (Var name _) types params body) = "(Fun (" ++ name ++ ") " ++ (show body) ++ ")"
    show (FunDefAnon types params body code) = "anon fun : " ++ (show types)
    show (Apply e1 e2) = "(" ++ show (e1) ++ " " ++ show (e2) ++ ")"
    show (TypeSig sig expr) = (show expr) ++ " : " ++ (show sig)
    show (ListLit exprs _) = "[" ++ (intercalate "," (map show exprs)) ++ "]"
    show (BoolLit b) = show b
    show (If condEx thenEx elseEx code) = "if " ++ show (condEx) ++ " then " ++ show thenEx ++ " else " ++ show elseEx
    show (Case exprs matches types) = "(Case " ++ (show matches) ++ ")"
    show (TypeDef (Var name _) types) = "(TypeDef " ++ name ++ " " ++ show types ++ ")"
    show (StructType types) = "(StructType " ++ show types ++ ")"
    show (StructValue sv) = Map.foldrWithKey f (show (fromJust $ Map.lookup "type" sv)) sv
      where f k a result = if k == "type" then result else result ++ " " ++ k ++ ":" ++ (show a)
    show (Call name _) = "(Call " ++ name ++ ")"
    show (TypeLit types) = "(TypeLit " ++ show types ++ ")"
  instance Eq Expr where
    (IntLit i1) == (IntLit i2) = i1 == i2
    (StrLit s1) == (StrLit s2) = s1 == s2
    (ListLit l1 c1) == (ListLit l2 c2) = l1 == l2
    (BoolLit b1) == (BoolLit b2) = b1 == b2
    e1 == e2 = trace (show (e1,e2)) $ False

  typeOf' :: Expr -> RecList String
  typeOf' (IntLit i) = Elem "Int"
  typeOf' (StrLit s) = Elem "String"
  typeOf' (BoolLit b) = Elem "Bool"
  typeOf' (DoubleLit b) = Elem "Double"
  typeOf' (TypeSig sig _) = sig
  typeOf' (Fun sig _ _ _) = sig
  typeOf' (ListLit (e:es) _) = Elems [Elem "List", typeOf' e]
  typeOf' (ListLit [] _) = Elems [Elem "List", Elem "a"]
  typeOf' (StructValue s) = case Map.lookup "type" s of
    Just (StrLit str) -> Elem str
    Nothing           -> error "type not defined in struct value"

  -- パターンマッチの元になる式と、マッチさせる対象のリストから、変数または関数とそれに対する式のリストの組を返す
  -- 例: a 2 [e;es] に 10 2 [1,2,3] をマッチさせる場合、(["a", "e", "es"], [IntLit 10, IntLit 1, ListLit [2,3]]) を返す
  paramsAndArgs :: [Expr] -> [Expr] -> ([String], [Expr])
  paramsAndArgs [] [] = ([],[])
  paramsAndArgs ((Var v _):e1s) (e:e2s) = let rests = paramsAndArgs e1s e2s
                                      in (v : (fst rests), e : (snd rests))
  paramsAndArgs (ListLit [(Var h _),(Var t _)] c1 : e1s) (ListLit (e2:e2') c2 : e2s) = 
    let rests = paramsAndArgs e1s e2s
    in (h : t : (fst rests), e2 : (ListLit e2' c2) : (snd rests))
  paramsAndArgs (e1:e1s) (e2:e2s) = paramsAndArgs e1s e2s
  