-- original author: unnohideyuki
-- referenced from: https://github.com/unnohideyuki/Tiger-in-Haskell/blob/master/chap5/Absyn.hs
module Ast where

import           Symbol                         ( Symbol )

data Pos = Pos { line :: Int, column :: Int }

instance Show Pos where
  show Pos { line = l, column = c } = (show l) ++ ":" ++ (show c) ++ ": "

data Var
  = SimpleVar Symbol Pos
  | FieldVar Var Symbol Pos
  | SubscriptVar Var Expr Pos
  deriving(Show)

data Expr
  = VarExpr Var
  | NilExpr
  | IntExpr Integer Pos
  | StringExpr String Pos
  | CallExpr { func :: Symbol, args :: [Expr], pos :: Pos }
  | OpExpr { op :: Op, lhs :: Expr, rhs :: Expr, pos :: Pos}
  | RecordExpr { fields :: [(Symbol, Expr, Pos)], typ :: Symbol, pos :: Pos }
  | SeqExpr [Expr]
  | AssignExpr { vvar :: Var, expr :: Expr, pos :: Pos }
  | IfExpr  { test :: Expr, then_expr :: Expr, else_expr :: Maybe Expr, pos :: Pos }
  | WhileExpr { test :: Expr, body :: Expr, pos :: Pos }
  | ForExpr { var :: Symbol
            , escape :: Bool
            , begin :: Expr
            , end :: Expr
            , body :: Expr
            , pos :: Pos }
  | BreakExpr Pos
  | LetExpr { decs :: [Dec], body :: Expr, pos :: Pos }
  | ArrayExpr { typ :: Symbol, size :: Expr, init :: Expr, pos :: Pos }
  deriving(Show)

data Dec
  = FunctionDec [FuncDec]
  | VarDec { name' :: Symbol
           , escape' :: Bool
           , typ' :: Maybe Symbol
           , init' :: Expr
           , pos' :: Pos }
  | TypeDec [(Symbol, Ty, Pos)]
  deriving(Show)

data Ty
  = NameTy Symbol Pos
  | RecordTy [Field] Pos
  | ArrayTy Symbol Pos
  deriving(Show)

data Op
  = PlusOp
  | MinusOp
  | MulOp
  | DivOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  deriving(Show)

data Field = Field { field_name :: Symbol
                   , field_esc :: Bool
                   , field_typ :: Symbol
                   , field_pos :: Pos }
                   deriving(Show)

data FuncDec = FuncDec { name :: Symbol
                       , params :: [Field]
                       , result :: Maybe Symbol
                       , func_body :: Expr
                       , func_pos :: Pos }
                       deriving(Show)



