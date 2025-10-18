module CompilationEngine where

import JackTokenizer

data AST
  = Class String [AST] [AST]
  | VarDec String String [String]
  | Subroutine String String String [(String, String)] AST
  | SubroutineBody [AST] [AST]
  | LetStatement String (Maybe AST) AST
  | IfStatement AST [AST] (Maybe [AST])
  | WhileStatement AST [AST]
  | DoStatement AST
  | ReturnStatement (Maybe AST)
  | Expression AST [(Token, AST)]
  | ExpressionList (Maybe (AST, [AST]))
  | FunctionCall String AST
  | SubroutineCall String String AST
  | TermInt Int
  | TermString String
  | TermConstant String
  | TermVar String
  | TermObj String AST
  | TermCall AST
  | TermExpr AST
  | TermOp String AST
  deriving (Show, Eq)

consumeToken :: [Token] -> Token -> [Token]
consumeToken (x : xs) token
  | x == token = xs

peekToken :: [Token] -> Maybe Token
peekToken [] = Nothing
peekToken (x : _) = Just x

compileClass :: [Token] -> AST
compileClass state =
  let state2 = consumeToken state (Keyword CLASS)
      (className, state3) = compileIdentifier state2
      state4 = consumeToken state3 (Symbol LCB)
      (classVarDecs, state5) = compileClassVarDecs state4
      (subroutineDec, state6) = compileSubroutineDecs state5
      state7 = consumeToken state6 (Symbol RCB)
   in Class className classVarDecs subroutineDec

compileIdentifier :: [Token] -> (String, [Token])
compileIdentifier ((Identifier i) : xs) = (i, xs)

compileClassVarDecs :: [Token] -> ([AST], [Token])
compileClassVarDecs state =
  case peekToken state of
    Just (Keyword VAR) -> ([], state)
    _ -> compileVarDecs state

compileVarDecs :: [Token] -> ([AST], [Token])
compileVarDecs state =
  case peekToken state of
    Just (Keyword STATIC) ->
      let (varDec, restState) = compileVarDec state STATIC
          (nextVarDecs, finalState) = compileClassVarDecs restState
       in (varDec : nextVarDecs, finalState)
    Just (Keyword FIELD) ->
      let (varDec, restState) = compileVarDec state FIELD
          (nextVarDecs, finalState) = compileClassVarDecs restState
       in (varDec : nextVarDecs, finalState)
    Just (Keyword VAR) ->
      let (varDec, restState) = compileVarDec state VAR
          (nextVarDecs, finalState) = compileVarDecs restState
       in (varDec : nextVarDecs, finalState)
    _ -> ([], state)

compileVarDec :: [Token] -> Keyword -> (AST, [Token])
compileVarDec state keyword =
  let state2 = consumeToken state (Keyword keyword)
      (varType, state3) = compileType state2
      (varName, state4) = compileIdentifier state3
      (restVars, state5) = compileRestVars state4
      state6 = consumeToken state5 (Symbol SEMICOLON)
   in (VarDec (show keyword) varType (varName : restVars), state6)

compileRestVars :: [Token] -> ([String], [Token])
compileRestVars state =
  case peekToken state of
    Just (Symbol COMMA) ->
      let state2 = tail state
          (varName, state3) = compileIdentifier state2
          (restVars, state4) = compileRestVars state3
       in (varName : restVars, state4)
    _ -> ([], state)

compileType :: [Token] -> (String, [Token])
compileType state =
  case peekToken state of
    Just (Keyword BOOLEAN) -> ("bool", tail state)
    Just (Keyword INT) -> ("int", tail state)
    Just (Keyword CHAR) -> ("char", tail state)
    Just (Identifier i) -> (i, tail state)

compileSubroutineDecs :: [Token] -> ([AST], [Token])
compileSubroutineDecs state =
  case peekToken state of
    Just (Keyword CONSTRUCTOR) ->
      let (subDec, restState) = compileSubroutineDec state CONSTRUCTOR
          (restSubs, finalState) = compileSubroutineDecs restState
       in (subDec : restSubs, finalState)
    Just (Keyword FUNCTION) ->
      let (subDec, restState) = compileSubroutineDec state FUNCTION
          (restSubs, finalState) = compileSubroutineDecs restState
       in (subDec : restSubs, finalState)
    Just (Keyword METHOD) ->
      let (subDec, restState) = compileSubroutineDec state METHOD
          (nextSubDecs, finalState) = compileSubroutineDecs restState
       in (subDec : nextSubDecs, finalState)
    _ -> ([], state)

compileSubroutineDec :: [Token] -> Keyword -> (AST, [Token])
compileSubroutineDec state keyword =
  let state2 = tail state
      (returnType, state3) = compileReturnType state2
      (subroutineName, state4) = compileIdentifier state3
      state5 = consumeToken state4 (Symbol LP)
      (parameterList, state6) = compileParameterList state5
      state7 = consumeToken state6 (Symbol RP)
      (subroutineBody, state8) = compileSubroutineBody state7
   in (Subroutine (show keyword) returnType subroutineName parameterList subroutineBody, state8)

compileReturnType :: [Token] -> (String, [Token])
compileReturnType state =
  case peekToken state of
    Just (Keyword VOID) -> ("void", tail state)
    _ -> compileType state

compileParameterList :: [Token] -> ([(String, String)], [Token])
compileParameterList state =
  case peekToken state of
    Just (Symbol RP) -> ([], state)
    Just (Symbol COMMA) ->
      let state2 = tail state
          (varType, state3) = compileType state2
          (varName, state4) = compileIdentifier state3
          (restVars, state5) = compileParameterList state4
       in ((varType, varName) : restVars, state5)
    _ ->
      let (varType, state2) = compileType state
          (varName, state3) = compileIdentifier state2
          (restVars, state4) = compileParameterList state3
          state5 = consumeToken state4 (Symbol RP)
       in ((varType, varName) : restVars, state5)

compileSubroutineVarDecs :: [Token] -> ([AST], [Token])
compileSubroutineVarDecs state =
  case peekToken state of
    Just (Keyword VAR) -> compileVarDecs state
    _ -> ([], state)

compileSubroutineBody :: [Token] -> (AST, [Token])
compileSubroutineBody state =
  let state2 = consumeToken state (Symbol LCB)
      (varDec, state3) = compileSubroutineVarDecs state2
      (statements, state4) = compileStatements state3
      state5 = consumeToken state4 (Symbol RCB)
   in (SubroutineBody varDec statements, state5)

compileStatements :: [Token] -> ([AST], [Token])
compileStatements state =
  case peekToken state of
    Just (Keyword LET) ->
      let (statement, restState) = compileLet state
          (nextStatements, finalState) = compileStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword IF) ->
      let (statement, restState) = compileIf state
          (nextStatements, finalState) = compileStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword WHILE) ->
      let (statement, restState) = compileWhile state
          (nextStatements, finalState) = compileStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword DO) ->
      let (statement, restState) = compileDo state
          (nextStatements, finalState) = compileStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword RETURN) ->
      let (statement, restState) = compileReturn state
          (nextStatements, finalState) = compileStatements restState
       in (statement : nextStatements, finalState)
    _ -> ([],state)

compileLet :: [Token] -> (AST, [Token])
compileLet state =
  let state2 = tail state
      (varName, state3) = compileIdentifier state2
      (varExpression, state4) = compileLetExpression state3
      state5 = consumeToken state4 (Symbol EQUAL)
      (expression, state6) = compileExpression state5
      state7 = consumeToken state6 (Symbol SEMICOLON)
   in (LetStatement varName varExpression expression, state7)

compileLetExpression :: [Token] -> (Maybe AST, [Token])
compileLetExpression state =
  case peekToken state of
    Just (Symbol LB) ->
      let state2 = tail state
          (expression, state3) = compileExpression state2
          state4 = consumeToken state3 (Symbol RB)
       in (Just expression, state4)
    _ -> (Nothing, state)

compileIf :: [Token] -> (AST, [Token])
compileIf state =
  let state2 = tail state
      state3 = consumeToken state2 (Symbol LP)
      (expression, state4) = compileExpression state3
      state5 = consumeToken state4 (Symbol RP)
      state6 = consumeToken state5 (Symbol LCB)
      (ifStatements, state7) = compileStatements state6
      state8 = consumeToken state7 (Symbol RCB)
      (elseStatements, state9) = compileElse state8
   in (IfStatement expression ifStatements elseStatements, state9)

compileElse :: [Token] -> (Maybe [AST], [Token])
compileElse state =
  case peekToken state of
    Just (Keyword ELSE) ->
      let state2 = tail state
          state3 = consumeToken state2 (Symbol LCB)
          (statements, state4) = compileStatements state3
          state5 = consumeToken state4 (Symbol RCB)
       in (Just statements, state5)

compileWhile :: [Token] -> (AST, [Token])
compileWhile state =
  let state2 = tail state
      state3 = consumeToken state2 (Symbol LP)
      (expressions, state4) = compileExpression state3
      state5 = consumeToken state4 (Symbol RP)
      state6 = consumeToken state5 (Symbol LCB)
      (statements, state7) = compileStatements state6
      state8 = consumeToken state7 (Symbol RCB)
   in (WhileStatement expressions statements, state8)

compileDo :: [Token] -> (AST, [Token])
compileDo state =
  let state2 = tail state
      (subroutineCall, state3) = compileSubroutineCall state2
      state4 = consumeToken state3 (Symbol SEMICOLON)
   in (DoStatement subroutineCall, state4)

compileReturn :: [Token] -> (AST, [Token])
compileReturn state =
  case peekToken . tail $ state of
    Just (Symbol SEMICOLON) -> (ReturnStatement Nothing, tail . tail $ state)
    _ ->
      let state2 = tail state
          (expression, state3) = compileExpression state2
          state4 = consumeToken state3 (Symbol SEMICOLON)
       in (ReturnStatement (Just expression), state4)

compileExpression :: [Token] -> (AST, [Token])
compileExpression state =
  let (firstTerm, state2) = compileTerm state
      (restExpression, state3) = compileExpressionRest state2
   in (Expression firstTerm restExpression, state3)

ops :: [Token]
ops =
  [ Symbol PLUS,
    Symbol MINUS,
    Symbol ASTERISK,
    Symbol SLASH,
    Symbol AMPERSAND,
    Symbol BAR,
    Symbol LESS_THAN,
    Symbol GREATER_THAN,
    Symbol EQUAL
  ]

maybeElem :: Maybe Token -> [Token] -> Bool
maybeElem Nothing _ = False
maybeElem (Just x) xs = x `elem` xs

maybeJust :: Maybe a -> a
maybeJust (Just x) = x

compileExpressionRest :: [Token] -> ([(Token, AST)], [Token])
compileExpressionRest state
  | maybeElem (peekToken state) ops =
      let state2 = tail state
          (term, state3) = compileTerm state2
          (restTerms, state4) = compileExpressionRest state3
       in ((maybeJust (peekToken state), term) : restTerms, state4)
  | otherwise = ([], state)

compileExpressionList :: [Token] -> (AST, [Token])
compileExpressionList state =
  case peekToken state of
    Just (Symbol RP) -> (ExpressionList Nothing, state)
    _ ->
      let (expression, state2) = compileExpression state
          (restExpression, state3) = compileExpressionListRest state2
       in (ExpressionList (Just (expression, restExpression)), state3)

compileExpressionListRest :: [Token] -> ([AST], [Token])
compileExpressionListRest state =
  case peekToken state of
    Just (Symbol COMMA) ->
      let state2 = tail state
          (expression, state3) = compileExpression state2
          (expressionRest, state4) = compileExpressionListRest state3
       in (expression : expressionRest, state4)
    _ -> ([], state)

peekSecondToken :: [Token] -> Maybe Token
peekSecondToken [] = Nothing
peekSecondToken [x] = Nothing
peekSecondToken (x : y : ys) = Just y

compileSubroutineCall :: [Token] -> (AST, [Token])
compileSubroutineCall state =
  case peekSecondToken state of
    Just (Symbol LP) ->
      let (subroutineName, state2) = compileIdentifier state
          state3 = tail state2
          (expressions, state4) = compileExpressionList state3
          state5 = consumeToken state4 (Symbol RP)
       in (FunctionCall subroutineName expressions, state5)
    Just (Symbol POINT) ->
      let (typeName, state2) = compileIdentifier state
          state3 = tail state2
          (subroutineName, state4) = compileIdentifier state3
          state5 = consumeToken state4 (Symbol LP)
          (expressions, state6) = compileExpressionList state5
          state7 = consumeToken state6 (Symbol RP)
       in (SubroutineCall typeName subroutineName expressions, state7)
    _ -> error ""

peekTwo :: [Token] -> Maybe (Token, Token)
peekTwo [] = Nothing
peekTwo [x] = Nothing
peekTwo (x : y : xs) = Just (x, y)

keyConstant :: [Keyword]
keyConstant = [TRUE, FALSE, NULL, THIS]

compileTerm :: [Token] -> (AST, [Token])
compileTerm state =
  case peekTwo state of
    Just (IntVal i, _) -> (TermInt i, tail state)
    Just (StrVal i, _) -> (TermString i, tail state)
    Just (Symbol LP, _) ->
      let state2 = tail state
          (expression, state3) = compileExpression state2
          state4 = consumeToken state3 (Symbol RP)
       in (TermExpr expression, state4)
    Just (Symbol i, _) ->
      if i == MINUS || i == TILDE
        then
          let state2 = tail state
              (term, state3) = compileTerm state2
           in (TermOp (show i) term, state3)
        else error ""
    Just (Keyword i, _) ->
      if i `elem` keyConstant
        then (TermConstant (show i), tail state)
        else error ""
    Just (Identifier i, Symbol LB) ->
      let state2 = tail . tail $ state
          (expression, state3) = compileExpression state2
          state4 = consumeToken state3 (Symbol RB)
       in (TermObj i expression, state4)
    Just (Identifier i, Symbol j) ->
      if j == POINT || j == LP
        then
          let (subroutine, state2) = compileSubroutineCall state
           in (TermCall subroutine, state2)
        else (TermVar i, tail state)

