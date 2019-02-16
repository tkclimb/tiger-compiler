module Env where

import           Types                          ( Ty )
import           Symbol                         ( Table
                                                , empty
                                                )

data EnvEntry = Var {ty :: Ty}
              | Func {formals :: [Ty], result :: Ty}
              deriving (Eq, Show)

type TEnv = Table Ty
type VEnv = Table EnvEntry

baseTEnv :: TEnv
baseTEnv = empty

baseVEnv :: VEnv
baseVEnv = empty



