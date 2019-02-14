{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( doParse
    )
where

import           Parser                         ( parse )
import           Lexer                          ( alexScanTokens
                                                , prettyToken
                                                , printTokens
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

doParse = do
    s <- getContents
    let tokens = alexScanTokens s
    let ast = parse tokens
    print ast
