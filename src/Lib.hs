module Lib (doParse) where

import Data.Typeable
import Parser (parse)
import Lexer  (alexScanTokens,
               printTokens)
import Semantics (transExpr)
import Env (baseVEnv, baseTEnv)

doParse = do
    s <- getContents
    let tokens = alexScanTokens s
    let ast    = parse tokens
    putStrLn ("type of ast is " ++ (show (typeOf ast)))
    let exprty =  transExpr baseVEnv baseTEnv ast
    print exprty
    print ast
