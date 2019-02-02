module Expr where

  import Data.Char
  import Data.IORef
  import Data.List (intercalate)
  import Data.Map as Map hiding (map, foldr, take)
  import Data.Maybe
  import Debug.Trace

  import DeepList
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
    | Call Name

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
    show (StructValue sv) = Map.foldrWithKey f (show (fromJust $ Map.lookup "type" sv)) sv
      where f k a result = if k == "type" then result else result ++ " " ++ k ++ ":" ++ (show a)
    show (Call name) = "(Call " ++ name ++ ")"
  
  instance Eq Expr where
    (IntLit i1) == (IntLit i2) = i1 == i2
    (StrLit s1) == (StrLit s2) = s1 == s2
    (ListLit l1) == (ListLit l2) = l1 == l2
    (BoolLit b1) == (BoolLit b2) = b1 == b2
    e1 == e2 = trace (show (e1,e2)) $ False

  -- showMembers :: Map String Expr -> String
  -- showMembers Map.empty = ""
  -- showMembers 

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

  matchCond :: [Expr] -> [Expr] -> Bool
  matchCond (IntLit i:e1s) (IntLit j:e2s) = i == j && matchCond e1s e2s
  matchCond ((IntLit i):e1s) ((Var v _):e2s) = matchCond e1s e2s
  matchCond (DoubleLit i:e1s) (DoubleLit j:e2s) = i == j && matchCond e1s e2s
  matchCond ((DoubleLit i):e1s) ((Var v _):e2s) = matchCond e1s e2s  
  matchCond ((ListLit l1):e1s) ((ListLit [Var h _, Var t _]):e2s) = matchCond e1s e2s
  matchCond ((ListLit l1):e1s) ((ListLit l2):e2s) = l1 == l2 && matchCond e1s e2s
  matchCond ((ListLit l):e1s) ((Var v _):e2s) = matchCond e1s e2s
  matchCond ((StructValue s):e1s) ((Var v _):e2s) = matchCond e1s e2s
  matchCond ((Fun _ _ _ _):e1s) ((Var v _):e2s) = matchCond e1s e2s
  matchCond [] [] = True
  matchCond e1 e2 = trace ("matchCond: " ++ show (e1,e2)) $ False

  paramsAndArgs :: [Expr] -> [Expr] -> ([String], [Expr])
  paramsAndArgs [] [] = ([],[])
  paramsAndArgs ((Var v _):e1s) (e:e2s) = let rests = paramsAndArgs e1s e2s
                                      in (v : (fst rests), e : (snd rests))
  paramsAndArgs (ListLit [(Var h _),(Var t _)]:e1s) (ListLit (e2:e2'):e2s) = 
    let rests = paramsAndArgs e1s e2s
    in (h : t : (fst rests), e2 : (ListLit e2') : (snd rests))
  paramsAndArgs (e1:e1s) (e2:e2s) = paramsAndArgs e1s e2s
  