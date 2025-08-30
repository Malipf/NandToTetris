module CompilationEngine where

import JackTokenizer (Keyword(..), Symbol(..), Token(..))

compileTokens :: Token -> String
compileTokens token =
  case token of
    (Keyword keyword)       -> "<keyword> " ++ (show keyword) ++ " </keyword>"
    (Symbol symbol)         -> "<symbol> " ++ (show symbol) ++ " </symbol>"
    (Identifier identifier) -> "<identifier> " ++ identifier ++ " </identifier>"
    (IntVal int)            -> "<integerConstant> " ++ (show int) ++ " </integerConstant>"
    (StrVal str)            -> "<stringConstant> " ++ str ++ " </stringConstant>"

{-
compileClass

compileClassVarDec

compileSubroutine

compileParameterList

compileSubroutineBody

compileVarDec

compileStatements

compileLet

compileIf

compileWhile

compileDo

compileReturn
-}
