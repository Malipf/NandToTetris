module CompilationEngine where

import JackTokenizer

data AST
  = Class String [AST] [AST]
  | ClassVarDec String String
  | SubroutineDec String String String [AST] AST
  | ParameterList [AST]
  | VarDec [AST]
  | Statements [AST]
  | DoStatement AST
  | LetStatement AST AST
  | WhileStatement AST AST
  | ReturnStatement (Maybe AST)
  | IfStatement AST AST (Maybe AST)
  | ExpressionList [AST]
  | Expression [AST]
  | Term AST
  | SubroutineCall String [AST]
  | Identifier String
  | Keyword String
  | Symbol String
  | IntVal Int
  | StrVal String
  | Void
  | Block [AST]
  deriving (Show, Eq)


data ParseState = ParseState { tokens :: [Token], astNodes :: [AST] }

peekToken :: ParseState -> Maybe Token
peekToken (ParseState (t:_) _) = Just t
peekToken _ = Nothing

consumeToken :: ParseState -> Token -> ParseState
consumeToken (ParseState (t:ts) nodes) expected
  | t == expected = ParseState ts (nodes ++ [toAST t])
  | otherwise     = error $ "Unexpected token: " ++ show t ++ ", expected " ++ show expected
consumeToken _ _  = error "Unexpected end of tokens"

toAST :: Token -> AST
toAST (Keyword k) = Keyword (show k)
toAST (Symbol s) = Symbol (show s)
toAST (Identifier i) = Identifier i
toAST (IntVal i) = IntVal i
toAST (StrVal s) = StrVal s

compileClass :: [Token] -> AST
compileClass tokens =
  let
    initialState = ParseState tokens []
    state1 = consumeToken initialState (Keyword CLASS)
    (className, state2) = compileIdentifier state1
    state3 = consumeToken state2 (Symbol LCB)
    (classVarDecs, state4) = compileClassVarDecs state3
    (subroutineDecs, state5) = compileSubroutineDecs state4
    state6 = consumeToken state5 (Symbol RCB)
  in
    Class className classVarDecs subroutineDecs

compileIdentifier :: ParseState -> (String, ParseState)
compileIdentifier (ParseState (Identifier i : ts) nodes) = (i, ParseState ts (nodes ++ [Identifier i]))
compileIdentifier _ = error "Expected Identifier"

compileClassVarDecs :: ParseState -> ([AST], ParseState)
compileClassVarDecs state =
  case peekToken state of
    Just (Keyword STATIC) ->
      let (varDec, restState) = compileVarDec state
(nextVarDecs, finalState) = compileClassVarDecs restState
      in (varDec : nextVarDecs, finalState)
         Just (Keyword FIELD) ->
let (varDec, restState) = compileVarDec state
                          (nextVarDecs, finalState) = compileClassVarDecs restState
  in (varDec : nextVarDecs, finalState)
     _ -> ([], state)


compileSubroutineDecs :: ParseState -> ([AST], ParseState)
compileSubroutineDecs state =
  case peekToken state of
    Just (Keyword CONSTRUCTOR) ->
      let (sub, rest) = compileSubroutineDec state
          (subs, final) = compileSubroutineDecs rest
      in (sub : subs, final)
    Just (Keyword FUNCTION) ->
      let (sub, rest) = compileSubroutineDec state
          (subs, final) = compileSubroutineDecs rest
      in (sub : subs, final)
    Just (Keyword METHOD) ->
      let (sub, rest) = compileSubroutineDec state
          (subs, final) = compileSubroutineDecs rest
      in (sub : subs, final)
    _ -> ([], state)


compileSubroutineDec :: ParseState -> (AST, ParseState)
compileSubroutineDec state =
  let
    (subType, state1) = compileKeyword state
    (returnType, state2) = compileType state1
    (subName, state3) = compileIdentifier state2
    state4 = consumeToken state3 (Symbol LP)
    (paramList, state5) = compileParameterList state4
    state6 = consumeToken state5 (Symbol RP)
    (body, finalState) = compileSubroutineBody state6
  in
    (SubroutineDec subType returnType subName paramList body, finalState)

compileType :: ParseState -> (String, ParseState)
compileType state =
  case peekToken state of
    Just (Keyword INT) -> ("int", consumeToken state (Keyword INT))
    Just (Keyword CHAR) -> ("char", consumeToken state (Keyword CHAR))
    Just (Keyword BOOLEAN) -> ("boolean", consumeToken state (Keyword BOOLEAN))
    Just (Keyword VOID) -> ("void", consumeToken state (Keyword VOID))
    Just (Identifier i) -> (i, consumeToken state (Identifier i))
    _ -> error "Expected a type keyword or identifier"

compileParameterList :: ParseState -> ([AST], ParseState)
compileParameterList state =
  case peekToken state of
    Just (Symbol RP) -> ([], state)
    _ ->
      let (typeVal, state1) = compileType state
          (nameVal, state2) = compileIdentifier state1
          (nextParams, finalState) =
            case peekToken state2 of
              Just (Symbol COMMA) -> compileParameterList (consumeToken state2 (Symbol COMMA))
              _ -> ([], state2)
      in (ParameterList (Keyword typeVal : Identifier nameVal : nextParams), finalState)

compileSubroutineBody :: ParseState -> (AST, ParseState)
compileSubroutineBody state =
  let
    state1 = consumeToken state (Symbol LCB)
    (varDecs, state2) = compileVarDecs state1
    (statements, state3) = compileStatements state2
    state4 = consumeToken state3 (Symbol RCB)
  in
    (Block (varDecs ++ statements), state4)


compileVarDecs :: ParseState -> ([AST], ParseState)
compileVarDecs state =
  case peekToken state of
    Just (Keyword VAR) ->
      let (varDec, rest) = compileVarDec state
          (nextVarDecs, final) = compileVarDecs rest
      in (varDec : nextVarDecs, final)
    _ -> ([], state)

compileVarDec :: ParseState -> (AST, ParseState)
compileVarDec state =
  let
    state1 = consumeToken state (Keyword VAR)
    (typeVal, state2) = compileType state1
    (varName, state3) = compileIdentifier state2
    (moreVars, state4) = compileVarList state3
    state5 = consumeToken state4 (Symbol SEMICOLON)
  in (VarDec (Keyword typeVal : Identifier varName : moreVars), state5)

compileVarList :: ParseState -> ([AST], ParseState)
compileVarList state =
  case peekToken state of
    Just (Symbol COMMA) ->
      let (varName, state1) = compileIdentifier (consumeToken state (Symbol COMMA))
          (moreVars, finalState) = compileVarList state1
      in (Identifier varName : moreVars, finalState)
    _ -> ([], state)


compileStatements :: ParseState -> ([AST], ParseState)
compileStatements state =
  case peekToken state of
    Just (Keyword LET) ->
      let (stmt, rest) = compileLet state
          (stmts, final) = compileStatements rest
      in (stmt : stmts, final)
    Just (Keyword IF) ->
      let (stmt, rest) = compileIf state
          (stmts, final) = compileStatements rest
      in (stmt : stmts, final)
    Just (Keyword WHILE) ->
      let (stmt, rest) = compileWhile state
          (stmts, final) = compileStatements rest
      in (stmt : stmts, final)
    Just (Keyword DO) ->
      let (stmt, rest) = compileDo state
          (stmts, final) = compileStatements rest
      in (stmt : stmts, final)
    Just (Keyword RETURN) ->
      let (stmt, rest) = compileReturn state
          (stmts, final) = compileStatements rest
      in (stmt : stmts, final)
    _ -> ([], state)

compileDo :: ParseState -> (AST, ParseState)
compileDo state =
  let
    state1 = consumeToken state (Keyword DO)
    (call, state2) = compileSubroutineCall state1
    state3 = consumeToken state2 (Symbol SEMICOLON)
  in (DoStatement call, state3)

compileLet :: ParseState -> (AST, ParseState)
compileLet state =
  let
    state1 = consumeToken state (Keyword LET)
    (varName, state2) = compileIdentifier state1
    (maybeArray, state3) = case peekToken state2 of
                             Just (Symbol LB) ->
                               let state4 = consumeToken state2 (Symbol LB)
                                   (expr, state5) = compileExpression state4
                                   state6 = consumeToken state5 (Symbol RB)
                               in (Just expr, state6)
                             _ -> (Nothing, state2)
    state7 = consumeToken state3 (Symbol EQUAL)
    (expr2, state8) = compileExpression state7
    state9 = consumeToken state8 (Symbol SEMICOLON)
  in (LetStatement (Identifier varName) maybeArray expr2, state9)


compileWhile :: ParseState -> (AST, ParseState)
compileWhile state =
  let
    state1 = consumeToken state (Keyword WHILE)
    state2 = consumeToken state1 (Symbol LP)
    (expr, state3) = compileExpression state2
    state4 = consumeToken state3 (Symbol RP)
    state5 = consumeToken state4 (Symbol LCB)
    (stmts, state6) = compileStatements state5
    state7 = consumeToken state6 (Symbol RCB)
  in (WhileStatement expr (Block stmts), state7)

compileReturn :: ParseState -> (AST, ParseState)
compileReturn state =
  let state1 = consumeToken state (Keyword RETURN)
      (maybeExpr, state2) =
        case peekToken state1 of
          Just (Symbol SEMICOLON) -> (Nothing, state1)
          _ -> let (expr, rest) = compileExpression state1
               in (Just expr, rest)
      state3 = consumeToken state2 (Symbol SEMICOLON)
  in (ReturnStatement maybeExpr, state3)

compileIf :: ParseState -> (AST, ParseState)
compileIf state =
  let state1 = consumeToken state (Keyword IF)
      state2 = consumeToken state1 (Symbol LP)
      (expr, state3) = compileExpression state2
      state4 = consumeToken state3 (Symbol RP)
      state5 = consumeToken state4 (Symbol LCB)
      (ifStmts, state6) = compileStatements state5
      state7 = consumeToken state6 (Symbol RCB)
      (elseBlock, finalState) = case peekToken state7 of
                                  Just (Keyword ELSE) ->
                                    let state8 = consumeToken state7 (Keyword ELSE)
                                        state9 = consumeToken state8 (Symbol LCB)
                                        (elseStmts, state10) = compileStatements state9
                                        state11 = consumeToken state10 (Symbol RCB)
                                    in (Just (Block elseStmts), state11)
                                  _ -> (Nothing, state7)
  in (IfStatement expr (Block ifStmts) elseBlock, finalState)

compileExpression :: ParseState -> (AST, ParseState)
compileExpression state =
  let (term, state1) = compileTerm state
      (exprTerms, finalState) = compileOpTerm state1
  in (Expression (term : exprTerms), finalState)

compileOpTerm :: ParseState -> ([AST], ParseState)
compileOpTerm state =
  case peekToken state of
    Just (Symbol PLUS) ->
      let state1 = consumeToken state (Symbol PLUS)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "+" : term : nextTerms, finalState)
    Just (Symbol MINUS) ->
      let state1 = consumeToken state (Symbol MINUS)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "-" : term : nextTerms, finalState)
    Just (Symbol ASTERISK) ->
      let state1 = consumeToken state (Symbol ASTERISK)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "*" : term : nextTerms, finalState)
    Just (Symbol SLASH) ->
      let state1 = consumeToken state (Symbol SLASH)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "/" : term : nextTerms, finalState)
    Just (Symbol AMPERSAND) ->
      let state1 = consumeToken state (Symbol AMPERSAND)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "&" : term : nextTerms, finalState)
    Just (Symbol BAR) ->
      let state1 = consumeToken state (Symbol BAR)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "|" : term : nextTerms, finalState)
    Just (Symbol LESS_THAN) ->
      let state1 = consumeToken state (Symbol LESS_THAN)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "<" : term : nextTerms, finalState)
    Just (Symbol GREATER_THAN) ->
      let state1 = consumeToken state (Symbol GREATER_THAN)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol ">" : term : nextTerms, finalState)
    Just (Symbol EQUAL) ->
      let state1 = consumeToken state (Symbol EQUAL)
          (term, state2) = compileTerm state1
          (nextTerms, finalState) = compileOpTerm state2
      in (Symbol "=" : term : nextTerms, finalState)
    _ -> ([], state)


compileTerm :: ParseState -> (AST, ParseState)
compileTerm state =
  case peekToken state of
    Just (IntVal i) -> (Term (IntVal i), consumeToken state (IntVal i))
    Just (StrVal s) -> (Term (StrVal s), consumeToken state (StrVal s))
    Just (Keyword TRUE) -> (Term (Keyword "true"), consumeToken state (Keyword TRUE))
    Just (Keyword FALSE) -> (Term (Keyword "false"), consumeToken state (Keyword FALSE))
    Just (Keyword NULL) -> (Term (Keyword "null"), consumeToken state (Keyword NULL))
    Just (Keyword THIS) -> (Term (Keyword "this"), consumeToken state (Keyword THIS))
    Just (Symbol LP) ->
      let state1 = consumeToken state (Symbol LP)
          (expr, state2) = compileExpression state1
          state3 = consumeToken state2 (Symbol RP)
      in (Term expr, state3)
    Just (Symbol MINUS) ->
      let state1 = consumeToken state (Symbol MINUS)
          (term, state2) = compileTerm state1
      in (Term (Symbol "-" : term), state2)
    Just (Symbol TILDE) ->
      let state1 = consumeToken state (Symbol TILDE)
          (term, state2) = compileTerm state1
      in (Term (Symbol "~" : term), state2)
    Just (Identifier i) ->
      let state1 = consumeToken state (Identifier i)
      in case peekToken state1 of
        Just (Symbol LP) ->
          let (call, finalState) = compileSubroutineCall state
          in (Term call, finalState)
        Just (Symbol POINT) ->
          let (call, finalState) = compileSubroutineCall state
          in (Term call, finalState)
        Just (Symbol LB) ->
          let state2 = consumeToken state1 (Symbol LB)
              (expr, state3) = compileExpression state2
              state4 = consumeToken state3 (Symbol RB)
          in (Term (Identifier i : Symbol "[" : expr : Symbol "]"), state4)
        _ -> (Term (Identifier i), state1)
    _ -> error $ "Expected a term, but got: " ++ show (peekToken state)

compileSubroutineCall :: ParseState -> (AST, ParseState)
compileSubroutineCall state =
  let (name, state1) = compileIdentifier state
  in case peekToken state1 of
    Just (Symbol LP) ->
      let state2 = consumeToken state1 (Symbol LP)
          (exprs, state3) = compileExpressionList state2
          state4 = consumeToken state3 (Symbol RP)
      in (SubroutineCall name exprs, state4)
    Just (Symbol POINT) ->
      let state2 = consumeToken state1 (Symbol POINT)
          (subName, state3) = compileIdentifier state2
          state4 = consumeToken state3 (Symbol LP)
          (exprs, state5) = compileExpressionList state4
          state6 = consumeToken state5 (Symbol RP)
      in (SubroutineCall (name ++ "." ++ subName) exprs, state6)
    _ -> error "Expected '(' or '.' for a subroutine call"

compileExpressionList :: ParseState -> ([AST], ParseState)
compileExpressionList state =
  case peekToken state of
    Just (Symbol RP) -> ([], state)
    _ ->
      let (expr, state1) = compileExpression state
          (nextExprs, finalState) =
            case peekToken state1 of
              Just (Symbol COMMA) -> compileExpressionList (consumeToken state1 (Symbol COMMA))
              _ -> ([], state1)
      in (expr : nextExprs, finalState)

compileKeyword :: ParseState -> (String, ParseState)
compileKeyword state =
  case peekToken state of
    Just (Keyword k) -> (show k, consumeToken state (Keyword k))
    _ -> error "Expected a keyword"

-- XML generation
astToXML :: AST -> String
astToXML ast = toXML ast 0

toXML :: AST -> Int -> String
toXML (Class name varDecs subDecs) indent =
  indentStr indent ++ "<class>\n" ++
  indentStr (indent + 1) ++ "<keyword>class</keyword>\n" ++
  indentStr (indent + 1) ++ "<identifier>" ++ name ++ "</identifier>\n" ++
  indentStr (indent + 1) ++ "<symbol>{</symbol>\n" ++
  concatMap (\v -> toXML v (indent + 1)) varDecs ++
  concatMap (\s -> toXML s (indent + 1)) subDecs ++
  indentStr (indent + 1) ++ "<symbol>}</symbol>\n" ++
  indentStr indent ++ "</class>\n"

toXML (ClassVarDec varType varName) indent =
  indentStr indent ++ "<classVarDec>\n" ++
  indentStr (indent + 1) ++ "<keyword>" ++ varType ++ "</keyword>\n" ++
  indentStr (indent + 1) ++ "<identifier>" ++ varName ++ "</identifier>\n" ++
  indentStr (indent + 1) ++ "<symbol>;</symbol>\n" ++
  indentStr indent ++ "</classVarDec>\n"

toXML (SubroutineDec subType retType subName params body) indent =
  indentStr indent ++ "<subroutineDec>\n" ++
  indentStr (indent + 1) ++ "<keyword>" ++ subType ++ "</keyword>\n" ++
  indentStr (indent + 1) ++ "<keyword>" ++ retType ++ "</keyword>\n" ++
  indentStr (indent + 1) ++ "<identifier>" ++ subName ++ "</identifier>\n" ++
  toXML params (indent + 1) ++
  toXML body (indent + 1) ++
  indentStr indent ++ "</subroutineDec>\n"

toXML (ParameterList params) indent =
  indentStr indent ++ "<parameterList>\n" ++
  concatMap (\p -> toXML p (indent + 1)) params ++
  indentStr indent ++ "</parameterList>\n"

toXML (Block stmts) indent =
  indentStr indent ++ "<subroutineBody>\n" ++
  indentStr (indent + 1) ++ "<symbol>{</symbol>\n" ++
  concatMap (\s -> toXML s (indent + 1)) stmts ++
  indentStr (indent + 1) ++ "<symbol>}</symbol>\n" ++
  indentStr indent ++ "</subroutineBody>\n"

toXML (Statements stmts) indent =
  indentStr indent ++ "<statements>\n" ++
  concatMap (\s -> toXML s (indent + 1)) stmts ++
  indentStr indent ++ "</statements>\n"

toXML (DoStatement call) indent =
  indentStr indent ++ "<doStatement>\n" ++
  indentStr (indent + 1) ++ "<keyword>do</keyword>\n" ++
  toXML call (indent + 1) ++
  indentStr (indent + 1) ++ "<symbol>;</symbol>\n" ++
  indentStr indent ++ "</doStatement>\n"

toXML (LetStatement varName maybeArray expr) indent =
  indentStr indent ++ "<letStatement>\n" ++
  indentStr (indent + 1) ++ "<keyword>let</keyword>\n" ++
  toXML varName (indent + 1) ++
  (case maybeArray of
     Just arrExpr ->
       indentStr (indent + 1) ++ "<symbol>[</symbol>\n" ++
       toXML arrExpr (indent + 1) ++
       indentStr (indent + 1) ++ "<symbol>]</symbol>\n"
     Nothing -> "") ++
  indentStr (indent + 1) ++ "<symbol>=</symbol>\n" ++
  toXML expr (indent + 1) ++
  indentStr (indent + 1) ++ "<symbol>;</symbol>\n" ++
  indentStr indent ++ "</letStatement>\n"

toXML (WhileStatement expr block) indent =
  indentStr indent ++ "<whileStatement>\n" ++
  indentStr (indent + 1) ++ "<keyword>while</keyword>\n" ++
  indentStr (indent + 1) ++ "<symbol>(</symbol>\n" ++
  toXML expr (indent + 1) ++
  indentStr (indent + 1) ++ "<symbol>)</symbol>\n" ++
  toXML block (indent + 1) ++
  indentStr indent ++ "</whileStatement>\n"

toXML (ReturnStatement maybeExpr) indent =
  indentStr indent ++ "<returnStatement>\n" ++
  indentStr (indent + 1) ++ "<keyword>return</keyword>\n" ++
  (case maybeExpr of
     Just expr -> toXML expr (indent + 1)
     Nothing -> "") ++
  indentStr (indent + 1) ++ "<symbol>;</symbol>\n" ++
  indentStr indent ++ "</returnStatement>\n"

toXML (IfStatement expr ifBlock maybeElse) indent =
  indentStr indent ++ "<ifStatement>\n" ++
  indentStr (indent + 1) ++ "<keyword>if</keyword>\n" ++
  indentStr (indent + 1) ++ "<symbol>(</symbol>\n" ++
  toXML expr (indent + 1) ++
  indentStr (indent + 1) ++ "<symbol>)</symbol>\n" ++
  toXML ifBlock (indent + 1) ++
  (case maybeElse of
     Just elseBlock ->
       indentStr (indent + 1) ++ "<keyword>else</keyword>\n" ++
       toXML elseBlock (indent + 1)
     Nothing -> "") ++
  indentStr indent ++ "</ifStatement>\n"

toXML (ExpressionList exprs) indent =
  indentStr indent ++ "<expressionList>\n" ++
  concatMap (\e -> toXML e (indent + 1)) exprs ++
  indentStr indent ++ "</expressionList>\n"

toXML (Expression terms) indent =
  indentStr indent ++ "<expression>\n" ++
  concatMap (\t -> toXML t (indent + 1)) terms ++
  indentStr indent ++ "</expression>\n"

toXML (Term term) indent =
  indentStr indent ++ "<term>\n" ++
  toXML term (indent + 1) ++
  indentStr indent ++ "</term>\n"

toXML (SubroutineCall name exprs) indent =
  indentStr indent ++ "<subroutineCall>\n" ++
  indentStr (indent + 1) ++ "<identifier>" ++ name ++ "</identifier>\n" ++
  toXML (ExpressionList exprs) (indent + 1) ++
  indentStr indent ++ "</subroutineCall>\n"

toXML (Identifier i) indent = indentStr indent ++ "<identifier>" ++ i ++ "</identifier>\n"
toXML (Keyword k) indent = indentStr indent ++ "<keyword>" ++ k ++ "</keyword>\n"
toXML (Symbol s) indent = indentStr indent ++ "<symbol>" ++ s ++ "</symbol>\n"
toXML (IntVal i) indent = indentStr indent ++ "<integerConstant>" ++ show i ++ "</integerConstant>\n"
toXML (StrVal s) indent = indentStr indent ++ "<stringConstant>" ++ s ++ "</stringConstant>\n"

indentStr :: Int -> String
indentStr n = replicate (n * 2) ' '
