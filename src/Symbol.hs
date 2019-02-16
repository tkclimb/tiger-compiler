module Symbol where

import qualified Data.Map                      as M

type Symbol = String
type Table = M.Map Symbol

empty = M.empty
insert table s v = M.insert s v table
lookup table s = M.lookup s table