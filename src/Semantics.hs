module Semantics where

import qualified Symbol                        as S
import qualified Env                           as E
import qualified Ast                           as A
import qualified Types                         as T

-- dummy
data ExprTy = ExprTy { ty :: T.Ty } deriving Show

data OpTy = ArithOp | CompOp | EqOp

actualTy :: T.Ty -> A.Pos -> T.Ty
actualTy ty pos = case ty of
  T.NameTy s t -> case t of
    Just ty' -> actualTy ty' pos
    Nothing  -> error $ show pos ++ "type not found: " ++ s
  T.ArrayTy ty' u -> T.ArrayTy (actualTy ty' pos) u
  _               -> ty

typeMismatch e a pos =
  error
    $  show pos
    ++ "Type mismatch: expected "
    ++ show e
    ++ ", actual "
    ++ show a

checkType t1 t2 pos =
  let t1' = actualTy t1 pos
      t2' = actualTy t2 pos
  in  if t1' /= t2'
        then case (t1', t2') of
          (T.RecordTy _ _, T.NilTy) -> True
          (T.NilTy, T.RecordTy _ _) -> True
          _ -> typeMismatch t1' t2' pos
        else True


transExpr :: E.VEnv -> E.TEnv -> A.Expr -> ExprTy
transExpr venv tenv =
  let trExpr A.NilExpr          = ExprTy T.NilTy

      trExpr (A.IntExpr    _ _) = ExprTy T.IntTy

      trExpr (A.StringExpr _ _) = ExprTy T.StringTy

      trExpr A.OpExpr { A.op = op, A.lhs = lhs, A.rhs = rhs, A.pos = pos } =
          let ExprTy { ty = lty } = trExpr lhs
              ExprTy { ty = rty } = trExpr rhs

              opKind op = case op of
                A.PlusOp  -> ArithOp
                A.MinusOp -> ArithOp
                A.MulOp   -> ArithOp
                A.DivOp   -> ArithOp
                A.LtOp    -> CompOp
                A.GtOp    -> CompOp
                A.LeOp    -> CompOp
                A.GeOp    -> CompOp
                A.EqOp    -> CompOp
                A.NeqOp   -> CompOp

              checkInt ty pos = case ty of
                T.IntTy -> True
                _       -> error $ show pos ++ ": Integer type required..."

              checkArith = checkInt lty pos && checkInt rty pos

              checkEq = case lty of
                T.IntTy -> checkType lty rty pos

              checkComp  = case lty of
                T.IntTy -> checkType lty rty pos
                T.StringTy -> checkType lty rty pos
                _ -> error $ show pos ++ "lty(" ++ show lty ++ ") is different from rty(" ++ show rty ++ ")..."

              checkOp = 
                case opKind op of
                  ArithOp -> checkArith
                  CompOp -> checkComp
                  EqOp -> checkEq
              
          in  
            if checkOp then 
              ExprTy T.IntTy
            else
              error "not reachable"

  -- in  trExpr A.RecordExpr { }


-- checkType 


