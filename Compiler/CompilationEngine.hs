module CompilationEngine3 where


import JackTokenizer


data AST
  = Class String [AST] [AST]
  | VarDec String [String]
  deriving (Show, Eq)


consumeToken :: [Token] -> Token -> [Token]
consumeToken (x:xs) token
  | x == token = xs


peekToken :: [Token] -> Maybe Token
peekToken [] = Nothing
peekToken (x:_) = Just x


compileClass :: [Token] -> AST
compileClass state =
  let
    state2 = consumeToken state (Keyword CLASS)
    (className, state3) = compileIdentifier state2
    state4 = consumeToken state3 (Symbol LCB)
    (classVarDecs, state5) = compileClassVarDecs state4
    (subroutineDec, state6) = compileSubroutineDecs state5
    state7 = consumeToken state (Symbol RCB)
  in
    Class className classVarDecs subroutineDec


compileIdentifier :: [Token] -> (String, [Token])
compileIdentifier ((Identifier i):xs)= (i,xs)


compileClassVarDecs :: [Token] -> ([AST], [Token])
compileClassVarDecs state =
  case peekToken state of
    Just (Keyword STATIC) ->
      let (varDec, restState) = compileVarDec state STATIC
          (nextVarDecs, finalState) = compileClassVarDecs restState
      in  (varDec : nextVarDecs, finalState)
    Just (Keyword FIELD) ->
      let (varDec, restState) = compileVarDec state FIELD
          (nextVarDecs, finalState) = compileClassVarDecs restState
      in  (varDec : nextVarDecs, finalState)
    Nothing -> ([], state)


compileVarDec :: [Token] -> Keyword ->  (AST, [Token])
compileVarDec state keyword =
  let
    state2 = consumeToken state (Keyword keyword)
    (varType, state3) = compileType state2
    (varName, state4) = compileIdentifier state3
    (restVars, state5) = compileRestVars state4
    state6 = consumeToken state5 (Symbol SEMICOLON)
  in
    (VarDec varType (varName : restVars), state6)


compileRestVars :: [Token] -> ([String], [Token])
compileRestVars state =
  case peekToken state of
    Just (Symbol COMMA) ->
      let
        state2 = tail state
        (varName, state3) = compileIdentifier state2
        (restVars, state4) = compileRestVars state3
      in
        (varName : restVars, state4)
    _ -> ([], state)


compileType :: [Token] -> (String, [Token])
compileType state =
  case peekToken state of
    Just (Keyword BOOLEAN) -> ("bool", tail state)
    Just (Keyword INT)     -> ("int", tail state)
    Just (Keyword CHAR)    -> ("char", tail state)
    Just (Identifier i)    -> (i, tail state)


compileSubroutineDecs :: [Token] -> ([AST], [Token])
compileSubroutineDecs = undefined
    
