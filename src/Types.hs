module Types where

import           Symbol                         ( Symbol )

type Unique = Integer

data Ty 
  = NilTy
  | UnitTy
  | IntTy
  | StringTy
  | NameTy Symbol (Maybe Ty)
  | RecordTy [(Symbol, Ty)] Unique
  | ArrayTy Ty Unique
  deriving (Show, Eq)
