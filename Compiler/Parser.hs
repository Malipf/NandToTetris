module Compiler.Parser where

import Data.Char (toLower)
import Compiler.Lexer

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
  | Expression AST [(String, AST)]
  | ExpressionList (Maybe [AST])
  | ThisMethodCall String AST
  | SubroutineCall String String AST
  | TermInt Int
  | TermString String
  | TermConstant String
  | TermVar String
  | TermArr String AST
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

parseClass :: [Token] -> AST
parseClass state =
  let state2 = consumeToken state (Keyword CLASS)
      (className, state3) = parseIdentifier state2
      state4 = consumeToken state3 (Symbol LCB)
      (classVarDecs, state5) = parseClassVarDecs state4
      (subroutineDec, state6) = parseSubroutineDecs state5
      state7 = consumeToken state6 (Symbol RCB)
   in Class className classVarDecs subroutineDec

parseIdentifier :: [Token] -> (String, [Token])
parseIdentifier ((Identifier i) : xs) = (i, xs)

parseClassVarDecs :: [Token] -> ([AST], [Token])
parseClassVarDecs state =
  case peekToken state of
    Just (Keyword VAR) -> ([], state)
    _ -> parseVarDecs state

parseVarDecs :: [Token] -> ([AST], [Token])
parseVarDecs state =
  case peekToken state of
    Just (Keyword STATIC) ->
      let (varDec, restState) = parseVarDec state STATIC
          (nextVarDecs, finalState) = parseClassVarDecs restState
       in (varDec : nextVarDecs, finalState)
    Just (Keyword FIELD) ->
      let (varDec, restState) = parseVarDec state FIELD
          (nextVarDecs, finalState) = parseClassVarDecs restState
       in (varDec : nextVarDecs, finalState)
    Just (Keyword VAR) ->
      let (varDec, restState) = parseVarDec state VAR
          (nextVarDecs, finalState) = parseVarDecs restState
       in (varDec : nextVarDecs, finalState)
    _ -> ([], state)

parseVarDec :: [Token] -> Keyword -> (AST, [Token])
parseVarDec state keyword =
  let state2 = consumeToken state (Keyword keyword)
      (varType, state3) = parseType state2
      (varName, state4) = parseIdentifier state3
      (restVars, state5) = parseRestVars state4
      state6 = consumeToken state5 (Symbol SEMICOLON)
   in (VarDec (decapitilize . show $ keyword) varType (varName : restVars), state6)

parseRestVars :: [Token] -> ([String], [Token])
parseRestVars state =
  case peekToken state of
    Just (Symbol COMMA) ->
      let state2 = tail state
          (varName, state3) = parseIdentifier state2
          (restVars, state4) = parseRestVars state3
       in (varName : restVars, state4)
    _ -> ([], state)

parseType :: [Token] -> (String, [Token])
parseType state =
  case peekToken state of
    Just (Keyword BOOLEAN) -> ("boolean", tail state)
    Just (Keyword INT) -> ("int", tail state)
    Just (Keyword CHAR) -> ("char", tail state)
    Just (Identifier i) -> (i, tail state)

parseSubroutineDecs :: [Token] -> ([AST], [Token])
parseSubroutineDecs state =
  case peekToken state of
    Just (Keyword CONSTRUCTOR) ->
      let (subDec, restState) = parseSubroutineDec state CONSTRUCTOR
          (restSubs, finalState) = parseSubroutineDecs restState
       in (subDec : restSubs, finalState)
    Just (Keyword FUNCTION) ->
      let (subDec, restState) = parseSubroutineDec state FUNCTION
          (restSubs, finalState) = parseSubroutineDecs restState
       in (subDec : restSubs, finalState)
    Just (Keyword METHOD) ->
      let (subDec, restState) = parseSubroutineDec state METHOD
          (nextSubDecs, finalState) = parseSubroutineDecs restState
       in (subDec : nextSubDecs, finalState)
    _ -> ([], state)

parseSubroutineDec :: [Token] -> Keyword -> (AST, [Token])
parseSubroutineDec state keyword =
  let state2 = tail state
      (returnType, state3) = parseReturnType state2
      (subroutineName, state4) = parseIdentifier state3
      state5 = consumeToken state4 (Symbol LP)
      (parameterList, state6) = parseParameterList state5
      state7 = consumeToken state6 (Symbol RP)
      (subroutineBody, state8) = parseSubroutineBody state7
   in (Subroutine (decapitilize . show $ keyword) returnType subroutineName parameterList subroutineBody, state8)

parseReturnType :: [Token] -> (String, [Token])
parseReturnType state =
  case peekToken state of
    Just (Keyword VOID) -> ("void", tail state)
    _ -> parseType state

parseParameterList :: [Token] -> ([(String, String)], [Token])
parseParameterList state =
  case peekToken state of
    Just (Symbol RP) -> ([], state)
    Just (Symbol COMMA) ->
      let state2 = tail state
          (varType, state3) = parseType state2
          (varName, state4) = parseIdentifier state3
          (restVars, state5) = parseParameterList state4
       in ((varType, varName) : restVars, state5)
    _ ->
      let (varType, state2) = parseType state
          (varName, state3) = parseIdentifier state2
          (restVars, state4) = parseParameterList state3
       in ((varType, varName) : restVars, state4)

parseSubroutineVarDecs :: [Token] -> ([AST], [Token])
parseSubroutineVarDecs state =
  case peekToken state of
    Just (Keyword VAR) -> parseVarDecs state
    _ -> ([], state)

parseSubroutineBody :: [Token] -> (AST, [Token])
parseSubroutineBody state =
  let state2 = consumeToken state (Symbol LCB)
      (varDec, state3) = parseSubroutineVarDecs state2
      (statements, state4) = parseStatements state3
      state5 = consumeToken state4 (Symbol RCB)
   in (SubroutineBody varDec statements, state5)

parseStatements :: [Token] -> ([AST], [Token])
parseStatements state =
  case peekToken state of
    Just (Keyword LET) ->
      let (statement, restState) = parseLet state
          (nextStatements, finalState) = parseStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword IF) ->
      let (statement, restState) = parseIf state
          (nextStatements, finalState) = parseStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword WHILE) ->
      let (statement, restState) = parseWhile state
          (nextStatements, finalState) = parseStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword DO) ->
      let (statement, restState) = parseDo state
          (nextStatements, finalState) = parseStatements restState
       in (statement : nextStatements, finalState)
    Just (Keyword RETURN) ->
      let (statement, restState) = parseReturn state
          (nextStatements, finalState) = parseStatements restState
       in (statement : nextStatements, finalState)
    _ -> ([], state)

parseLet :: [Token] -> (AST, [Token])
parseLet state =
  let state2 = tail state
      (varName, state3) = parseIdentifier state2
      (iExpression, state4) = parseLetExpression state3
      state5 = consumeToken state4 (Symbol EQUAL)
      (oExpression, state6) = parseExpression state5
      state7 = consumeToken state6 (Symbol SEMICOLON)
   in (LetStatement varName iExpression oExpression, state7)

parseLetExpression :: [Token] -> (Maybe AST, [Token])
parseLetExpression state =
  case peekToken state of
    Just (Symbol LB) ->
      let state2 = tail state
          (expression, state3) = parseExpression state2
          state4 = consumeToken state3 (Symbol RB)
       in (Just expression, state4)
    _ -> (Nothing, state)

parseIf :: [Token] -> (AST, [Token])
parseIf state =
  let state2 = tail state
      state3 = consumeToken state2 (Symbol LP)
      (expression, state4) = parseExpression state3
      state5 = consumeToken state4 (Symbol RP)
      state6 = consumeToken state5 (Symbol LCB)
      (ifStatements, state7) = parseStatements state6
      state8 = consumeToken state7 (Symbol RCB)
      (elseStatements, state9) = parseElse state8
   in (IfStatement expression ifStatements elseStatements, state9)

parseElse :: [Token] -> (Maybe [AST], [Token])
parseElse state =
  case peekToken state of
    Just (Keyword ELSE) ->
      let state2 = tail state
          state3 = consumeToken state2 (Symbol LCB)
          (statements, state4) = parseStatements state3
          state5 = consumeToken state4 (Symbol RCB)
       in (Just statements, state5)
    _ -> (Nothing, state)

parseWhile :: [Token] -> (AST, [Token])
parseWhile state =
  let state2 = tail state
      state3 = consumeToken state2 (Symbol LP)
      (expressions, state4) = parseExpression state3
      state5 = consumeToken state4 (Symbol RP)
      state6 = consumeToken state5 (Symbol LCB)
      (statements, state7) = parseStatements state6
      state8 = consumeToken state7 (Symbol RCB)
   in (WhileStatement expressions statements, state8)

parseDo :: [Token] -> (AST, [Token])
parseDo state =
  let state2 = tail state
      (subroutineCall, state3) = parseSubroutineCall state2
      state4 = consumeToken state3 (Symbol SEMICOLON)
   in (DoStatement subroutineCall, state4)

parseReturn :: [Token] -> (AST, [Token])
parseReturn state =
  case peekToken . tail $ state of
    Just (Symbol SEMICOLON) -> (ReturnStatement Nothing, tail . tail $ state)
    _ ->
      let state2 = tail state
          (expression, state3) = parseExpression state2
          state4 = consumeToken state3 (Symbol SEMICOLON)
       in (ReturnStatement (Just expression), state4)

parseExpression :: [Token] -> (AST, [Token])
parseExpression state =
  let (firstTerm, state2) = parseTerm state
      (restExpression, state3) = parseExpressionRest state2
   in (Expression firstTerm restExpression, state3)

ops :: [Token]
ops =
  [ Symbol PLUS, Symbol MINUS, Symbol ASTERISK,
    Symbol SLASH, Symbol AMPERSAND, Symbol BAR,
    Symbol LESS_THAN,Symbol GREATER_THAN, Symbol EQUAL
  ]

maybeElem :: Maybe Token -> [Token] -> Bool
maybeElem Nothing _ = False
maybeElem (Just x) xs = x `elem` xs

maybeJust :: Maybe a -> a
maybeJust (Just x) = x

parseExpressionRest :: [Token] -> ([(String, AST)], [Token])
parseExpressionRest state
  | maybeElem (peekToken state) ops =
      let state2 = tail state
          (term, state3) = parseTerm state2
          (restTerms, state4) = parseExpressionRest state3
       in ((decapitilize . last . words . show . maybeJust . peekToken $ state, term) : restTerms, state4)
  | otherwise = ([], state)

parseExpressionList :: [Token] -> (AST, [Token])
parseExpressionList state =
  case peekToken state of
    Just (Symbol RP) -> (ExpressionList Nothing, state)
    _ ->
      let (expression, state2) = parseExpression state
          (restExpression, state3) = parseExpressionListRest state2
       in (ExpressionList (Just (expression : restExpression)), state3)

parseExpressionListRest :: [Token] -> ([AST], [Token])
parseExpressionListRest state =
  case peekToken state of
    Just (Symbol COMMA) ->
      let state2 = tail state
          (expression, state3) = parseExpression state2
          (expressionRest, state4) = parseExpressionListRest state3
       in (expression : expressionRest, state4)
    _ -> ([], state)

peekSecondToken :: [Token] -> Maybe Token
peekSecondToken [] = Nothing
peekSecondToken [x] = Nothing
peekSecondToken (x : y : ys) = Just y

parseSubroutineCall :: [Token] -> (AST, [Token])
parseSubroutineCall state =
  case peekSecondToken state of
    Just (Symbol LP) ->
      let (subroutineName, state2) = parseIdentifier state
          state3 = tail state2
          (expressions, state4) = parseExpressionList state3
          state5 = consumeToken state4 (Symbol RP)
       in (ThisMethodCall subroutineName expressions, state5)
    Just (Symbol POINT) ->
      let (typeName, state2) = parseIdentifier state
          state3 = tail state2
          (subroutineName, state4) = parseIdentifier state3
          state5 = consumeToken state4 (Symbol LP)
          (expressions, state6) = parseExpressionList state5
          state7 = consumeToken state6 (Symbol RP)
       in (SubroutineCall typeName subroutineName expressions, state7)

peekTwo :: [Token] -> Maybe (Token, Token)
peekTwo [] = Nothing
peekTwo [x] = Nothing
peekTwo (x : y : xs) = Just (x, y)

keyConstant :: [Keyword]
keyConstant = [TRUE, FALSE, NULL, THIS]

parseTerm :: [Token] -> (AST, [Token])
parseTerm state =
  case peekTwo state of
    Just (IntVal i, _) -> (TermInt i, tail state)
    
    Just (StrVal i, _) -> (TermString i, tail state)
    
    Just (Symbol LP, _) ->
      let state2 = tail state
          (expression, state3) = parseExpression state2
          state4 = consumeToken state3 (Symbol RP)
       in (TermExpr expression, state4)
    
    Just (Symbol i, _) ->
      if i == MINUS || i == TILDE
        then
          let state2 = tail state
              (term, state3) = parseTerm state2
           in (TermOp (decapitilize . show $ i) term, state3)
        else error ""
    
    Just (Keyword i, _) ->
      if i `elem` keyConstant
        then (TermConstant (decapitilize . show $ i), tail state)
        else error ""
    
    Just (Identifier i, Symbol LB) ->
      let state2 = tail . tail $ state
          (expression, state3) = parseExpression state2
          state4 = consumeToken state3 (Symbol RB)
       in (TermArr i expression, state4)
    
    Just (Identifier i, Symbol j) ->
      if j == POINT || j == LP
        then
          let (subroutine, state2) = parseSubroutineCall state
           in (TermCall subroutine, state2)
        else (TermVar i, tail state)

indent :: String -> String
indent = unlines . map ("  " ++) . lines

decapitilize :: String -> String
decapitilize "" = ""
decapitilize (x : xs) = toLower x : decapitilize xs

basicTag :: String -> String -> String
basicTag tag content = '<' : tag ++ "> " ++ content ++ " </" ++ tag ++ ">\n"

typeTag :: String -> String
typeTag x = case x of
  "boolean" -> basicTag "keyword" x
  "int" -> basicTag "keyword" x
  "char" -> basicTag "keyword" x
  "void" -> basicTag "keyword" x
  _ -> basicTag "identifier" x

paramTag :: [(String, String)] -> [String]
paramTag [] = []
paramTag ((t, n) : xs) = (typeTag t ++ basicTag "identifier" n) : paramTag xs

opTag :: String -> String
opTag x = case x of
  "plus" -> basicTag "symbol" "+"
  "minus" -> basicTag "symbol" "-"
  "asterisk" -> basicTag "symbol" "*"
  "slash" -> basicTag "symbol" "/"
  "ampersand" -> basicTag "symbol" "&amp;"
  "bar" -> basicTag "symbol" "|"
  "less_than" -> basicTag "symbol" "&lt;"
  "greater_than" -> basicTag "symbol" "&gt;"
  "equal" -> basicTag "symbol" "="

restTermTag :: [(String, AST)] -> [String]
restTermTag [] = []
restTermTag ((o, t) : xs) = (opTag o ++ astToXML t) : restTermTag xs

putComma :: [String] -> [String]
putComma [] = []
putComma [x] = [x]
putComma (x : xs) = x : basicTag "symbol" "," : putComma xs

astToXML :: AST -> String
astToXML (Class cName vars subs) =
  let classKeyword = basicTag "keyword" "class"
      className = basicTag "identifier" cName
      lcb = basicTag "symbol" "{"
      rcb = basicTag "symbol" "}"
      varDec = concatMap astToXML vars
      subDec = concatMap astToXML subs
      all = classKeyword ++ className ++ lcb ++ varDec ++ subDec ++ rcb
   in "<class>\n" ++ indent all ++ "</class>\n"

astToXML (VarDec "var" varType varNames) =
  let vk = basicTag "keyword" "var"
      vt = typeTag varType
      vn = map (basicTag "identifier") varNames
      end = basicTag "symbol" ";"
      all = vk ++ vt ++ (concat . putComma) vn ++ end
   in "<varDec>\n" ++ indent all ++ "</varDec>\n"

astToXML (VarDec varKeyword varType varNames) =
  let vk = basicTag "keyword" varKeyword
      vt = typeTag varType
      vn = map (basicTag "identifier") varNames
      end = basicTag "symbol" ";"
      all = vk ++ vt ++ (concat . putComma) vn ++ end
   in "<classVarDec>\n" ++ indent all ++ "</classVarDec>\n"

astToXML (Subroutine subType retType subName params body) =
  let st = basicTag "keyword" subType
      rt = typeTag retType
      sn = basicTag "identifier" subName
      lp = basicTag "symbol" "("
      ps = "<parameterList>\n"
      p = indent . concat . putComma . paramTag $ params
      pe = "</parameterList>\n"
      rp = basicTag "symbol" ")"
      sb = astToXML body
      all = st ++ rt ++ sn ++ lp ++ ps ++ p ++ pe ++ rp ++ sb
   in "<subroutineDec>\n" ++ indent all ++ "</subroutineDec>\n"

astToXML (SubroutineBody varDec statements) =
  let lcb = basicTag "symbol" "{"
      vd = concatMap astToXML varDec
      st = "<statements>\n" ++ indent (concatMap astToXML statements) ++ "</statements>\n"
      rcb = basicTag "symbol" "}"
      all = lcb ++ vd ++ st ++ rcb
   in "<subroutineBody>\n" ++ indent all ++ "</subroutineBody>\n"

astToXML (LetStatement varName Nothing expression) =
  let l = basicTag "keyword" "let"
      vn = basicTag "identifier" varName
      eq = basicTag "symbol" "="
      ex = astToXML expression
      sc = basicTag "symbol" ";"
      all = l ++ vn ++ eq ++ ex ++ sc
   in "<letStatement>\n" ++ indent all ++ "</letStatement>\n"

astToXML (LetStatement varName (Just iExpression) oExpression) =
  let l = basicTag "keyword" "let"
      vn = basicTag "identifier" varName
      lb = basicTag "symbol" "["
      iEx = astToXML iExpression
      rb = basicTag "symbol" "]"
      eq = basicTag "symbol" "="
      oEx = astToXML oExpression
      sc = basicTag "symbol" ";"
      all = l ++ vn ++ lb ++ iEx ++ rb ++ eq ++ oEx ++ sc
   in "<letStatement>\n" ++ indent all ++ "</letStatement>\n"

astToXML (IfStatement expression ifStatement Nothing) =
  let i = basicTag "keyword" "if"
      lp = basicTag "symbol" "("
      ex = astToXML expression
      rp = basicTag "symbol" ")"
      lcb = basicTag "symbol" "{"
      st = "<statements>\n" ++ indent (concatMap astToXML ifStatement) ++ "</statements>\n"
      rcb = basicTag "symbol" "}"
      all = i ++ lp ++ ex ++ rp ++ lcb ++ st ++ rcb
   in "<ifStatement>\n" ++ indent all ++ "</ifStatement>\n"

astToXML (IfStatement expression iStatement (Just eStatement)) =
  let i = basicTag "keyword" "if"
      lp = basicTag "symbol" "("
      ex = astToXML expression
      rp = basicTag "symbol" ")"
      lcb = basicTag "symbol" "{"
      is = "<statements>\n" ++ indent (concatMap astToXML iStatement) ++ "</statements>\n"
      rcb = basicTag "symbol" "}"
      e = basicTag "keyword" "else"
      lcb2 = basicTag "symbol" "{"
      es = "<statements>\n" ++ indent (concatMap astToXML eStatement) ++ "</statements>\n"
      rcb2 = basicTag "symbol" "}"
      all = i ++ lp ++ ex ++ rp ++ lcb ++ is ++ rcb ++ e ++ lcb2 ++ es ++ rcb2
   in "<ifStatement>\n" ++ indent all ++ "</ifStatement>\n"

astToXML (WhileStatement expression statement) =
  let w = basicTag "keyword" "while"
      lp = basicTag "symbol" "("
      ex = astToXML expression
      rp = basicTag "symbol" ")"
      lcb = basicTag "symbol" "{"
      st = "<statements>\n" ++ indent (concatMap astToXML statement) ++ "</statements>\n"
      rcb = basicTag "symbol" "}"
      all = w ++ lp ++ ex ++ rp ++ lcb ++ st ++ rcb
   in "<whileStatement>\n" ++ indent all ++ "</whileStatement>\n"

astToXML (DoStatement subCall) =
  let d = basicTag "keyword" "do"
      sc = astToXML subCall
      e = basicTag "symbol" ";"
      all = d ++ sc ++ e
   in "<doStatement>\n" ++ indent all ++ "</doStatement>\n"

astToXML (ReturnStatement Nothing) =
  let r = basicTag "keyword" "return"
      sc = basicTag "symbol" ";"
      all = r ++ sc
   in "<returnStatement>\n" ++ indent all ++ "</returnStatement>\n"

astToXML (ReturnStatement (Just expression)) =
  let r = basicTag "keyword" "return"
      ex = astToXML expression
      sc = basicTag "symbol" ";"
      all = r ++ ex ++ sc
   in "<returnStatement>\n" ++ indent all ++ "</returnStatement>\n"

astToXML (Expression first rest) =
  let t = astToXML first
      rt = concat . restTermTag $ rest
      all = t ++ rt
   in "<expression>\n" ++ indent all ++ "</expression>\n"

astToXML (ExpressionList Nothing) = "<expressionList>\n</expressionList>\n"

astToXML (ExpressionList (Just expressionList)) =
  let all = concat . putComma . map astToXML $ expressionList
   in "<expressionList>\n" ++ indent all ++ "</expressionList>\n"

astToXML (ThisMethodCall fName expressionList) =
  let fn = basicTag "identifier" fName
      lp = basicTag "symbol" "("
      exl = astToXML expressionList
      rp = basicTag "symbol" ")"
      all = fn ++ lp ++ exl ++ rp
   in all

astToXML (SubroutineCall fKind fName expressionList) =
  let fk = basicTag "identifier" fKind
      pt = basicTag "symbol" "."
      fn = basicTag "identifier" fName
      lp = basicTag "symbol" "("
      exl = astToXML expressionList
      rp = basicTag "symbol" ")"
      all = fk ++ pt ++ fn ++ lp ++ exl ++ rp
   in all

astToXML (TermInt i) = "<term>\n" ++ indent (basicTag "integerConstant" (show i)) ++ "</term>\n"

astToXML (TermString i) = "<term>\n" ++ indent (basicTag "stringConstant" i) ++ "</term>\n"

astToXML (TermConstant i) = "<term>\n" ++ indent (basicTag "keyword" i) ++ "</term>\n"

astToXML (TermVar i) = "<term>\n" ++ indent (basicTag "identifier" i) ++ "</term>\n"

astToXML (TermArr varName expression) =
  let vn = basicTag "identifier" varName
      lb = basicTag "symbol" "["
      ex = astToXML expression
      rb = basicTag "symbol" "]"
      all = vn ++ lb ++ ex ++ rb
   in "<term>\n" ++ indent all ++ "</term>\n"

astToXML (TermCall sub) = "<term>\n" ++ indent (astToXML sub) ++ "</term>\n"

astToXML (TermExpr expr) =
  let rp = basicTag "symbol" "("
      ex = astToXML expr
      lp = basicTag "symbol" ")"
      all = rp ++ ex ++ lp
   in "<term>\n" ++ indent all ++ "</term>\n"

astToXML (TermOp "minus" term) =
  let o = basicTag "symbol" "-"
      t = astToXML term
      all = o ++ t
   in "<term>\n" ++ indent all ++ "</term>\n"

astToXML (TermOp "tilde" term) =
  let o = basicTag "symbol" "~"
      t = astToXML term
      all = o ++ t
   in "<term>\n" ++ indent all ++ "</term>\n"
