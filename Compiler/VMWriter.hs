module VMWriter where

import CompilationEngine
import Data.Char (ord)

newtype Symbol = Symbol (String, String, String, Int)
  deriving (Eq, Show)

writeClass :: AST -> String
writeClass (Class name varDec subDec) =
  let (table, cf) = classTable varDec
      methodNames = map (\(Subroutine s _ n _ _) -> if s == "method" then n else "") subDec
      (subs, cs2) = writeSubs 0 cf methodNames table name subDec
   in subs

writeSubs :: Int -> String -> [String] -> [Symbol] -> String -> [AST] -> (String, Int)
writeSubs cs _ _ _ _ [] = ("", cs)
writeSubs cs cf mn table name (x : xs) =
  let (first, cs2) = writeSub cs cf mn table name x
      (rest, cs3) = writeSubs cs2 cf mn table name xs
   in (first ++ rest, cs3)

writeSub :: Int -> String -> [String] -> [Symbol] -> String -> AST -> (String, Int)
writeSub cs cf mn cTable className (Subroutine sKind sType sName params (SubroutineBody varDec statements)) =
  let (table, cl) = subTable cTable sKind className params varDec
      func = "function " ++ className ++ "." ++ sName ++ " " ++ cl ++ "\n"
      kindExtras =
        case sKind of
          "constructor" -> "push constant " ++ cf ++ "\n" ++ "call Memory.alloc 1\npop pointer 0\n"
          "method" -> "push argument 0\npop pointer 0\n"
          "function" -> ""
      (st, cs2) = writeStatements cs mn table className statements
   in (func ++ kindExtras ++ st, cs2)

writeExpression :: String -> [String] -> [Symbol] -> AST -> String
writeExpression cName mn table (Expression term []) = writeTerm cName mn table term
writeExpression cName mn table (Expression firstTerm rest) =
  writeTerm cName mn table firstTerm
    ++ concatMap (\(op, term) -> writeTerm cName mn table term ++ writeOp op) rest
writeExpression _ _ _ (ExpressionList Nothing) = ""
writeExpression cName mn table (ExpressionList (Just expressions)) =
  concatMap (writeExpression cName mn table) expressions

writeTerm :: String -> [String] -> [Symbol] -> AST -> String
writeTerm _ _ _ (TermInt i) = "push constant " ++ show i ++ "\n"
writeTerm _ _ _ (TermString s) =
  "push constant " ++ show (length s) ++ "\ncall String.new 1\n" ++ writeString s
writeTerm _ _ _ (TermConstant x) = writeConstant x
writeTerm _ _ table (TermVar x) = "push " ++ lookTable table x
writeTerm cName mn table (TermArr name expr) =
  "push "
    ++ lookTable table name
    ++ "\n"
    ++ writeExpression cName mn table expr
    ++ "add\npop pointer 1\npush that 0\n"
writeTerm cName mn table (TermCall (FunctionCall func expr)) =
  let pushThis = if func `elem` mn then "push pointer 0\n" else ""
      paramC = if func `elem` mn then show (len expr + 1) else show (len expr)
      rest =
        writeExpression cName mn table expr
          ++ "call "
          ++ cName
          ++ "."
          ++ func
          ++ " "
          ++ paramC
          ++ "\n"
   in pushThis ++ rest
writeTerm cName mn table (TermCall (SubroutineCall caller func expr)) =
  let callerType = lookTableType table caller
      isCons = callerType == Nothing
      caller2 = if isCons then Just caller else callerType
      (Just caller3) = caller2
      paramLength = if isCons then show (len expr) else show (len expr + 1)
      pushThis = if isCons then "" else "push " ++ lookTable table caller
   in pushThis
        ++ writeExpression cName mn table expr
        ++ "call "
        ++ caller3
        ++ "."
        ++ func
        ++ " "
        ++ paramLength
        ++ "\n"
writeTerm cName mn table (TermExpr expr) = writeExpression cName mn table expr
writeTerm cName mn table (TermOp op term) = writeTerm cName mn table term ++ writeUnOp op

writeConstant :: String -> String
writeConstant x =
  case x of
    "true" -> "push constant 1\nneg\n"
    "false" -> "push constant 0\n"
    "this" -> "push pointer 0\n"
    "null" -> "push constant 0\n"

writeOp :: String -> String
writeOp x =
  case x of
    "plus" -> "add\n"
    "minus" -> "sub\n"
    "asterisk" -> "call Math.multiply 2\n"
    "slash" -> "call Math.divide 2\n"
    "ampersand" -> "and\n"
    "bar" -> "or\n"
    "less_than" -> "lt\n"
    "greater_than" -> "gt\n"
    "equal" -> "eq\n"

writeUnOp :: String -> String
writeUnOp "tilde" = "not\n"
writeUnOp "minus" = "neg\n"

writeStatements :: Int -> [String] -> [Symbol] -> String -> [AST] -> (String, Int)
writeStatements cs _ _ _ [] = ("", cs)
writeStatements cs mn table cName (x : xs) =
  let (first, cs2) = writeStatement cs mn table cName x
      (rest, cs3) = writeStatements cs2 mn table cName xs
   in (first ++ rest, cs3)

writeStatement :: Int -> [String] -> [Symbol] -> String -> AST -> (String, Int)
writeStatement c mn table cName (LetStatement vName Nothing expression) =
  (writeExpression cName mn table expression ++ "pop " ++ lookTable table vName, c)
writeStatement c mn table cName (LetStatement name (Just iex) oex) =
  ( "push "
      ++ lookTable table name
      ++ writeExpression cName mn table iex
      ++ "add\n"
      ++ writeExpression cName mn table oex
      ++ "pop temp 0\npop pointer 1\npush temp 0\npop that 0\n",
    c
  )
writeStatement c _ table _ (ReturnStatement Nothing) = ("push constant 0\nreturn\n", c)
writeStatement c mn table cName (ReturnStatement (Just expr)) = (writeExpression cName mn table expr ++ "return\n", c)
writeStatement c mn table cName (DoStatement expr) =
  (writeExpression cName mn table (Expression (TermCall expr) []) ++ "pop temp 0\n", c)
writeStatement c mn table cName (IfStatement expression ifStatement Nothing) =
  let l1 = writeExpression cName mn table expression
      l2 = "not\n"
      l3 = "if-goto " ++ cName ++ ".else$" ++ show c ++ "\n"
      (l4, c2) = writeStatements (c + 1) mn table cName ifStatement
      l5 = "goto " ++ cName ++ ".if$" ++ show c ++ "\n"
      l6 = "label " ++ cName ++ ".else$" ++ show c ++ "\n"
      l7 = "label " ++ cName ++ ".if$" ++ show c ++ "\n"
   in (l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7, c2 + 1)
writeStatement c mn table cName (IfStatement expression ifStatement (Just elseStatement)) =
  let l1 = writeExpression cName mn table expression
      l2 = "not\n"
      l3 = "if-goto " ++ cName ++ ".else$" ++ show c ++ "\n"
      (l4, c2) = writeStatements (c + 1) mn table cName ifStatement
      l5 = "goto " ++ cName ++ ".if$" ++ show c ++ "\n"
      l6 = "label " ++ cName ++ ".else$" ++ show c ++ "\n"
      (l7, c3) = writeStatements c2 mn table cName elseStatement
      l8 = "label " ++ cName ++ ".if$" ++ show c ++ "\n"
   in (l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7 ++ l8, c3 + 1)
writeStatement c mn table cName (WhileStatement expression statements) =
  let l1 = "label " ++ cName ++ ".loop$" ++ show c ++ "\n"
      l2 = writeExpression cName mn table expression
      l3 = "not\n"
      l4 = "if-goto " ++ cName ++ ".break$" ++ show c ++ "\n"
      (l5, c2) = writeStatements (c + 1) mn table cName statements
      l6 = "goto " ++ cName ++ ".loop$" ++ show c ++ "\n"
      l7 = "label " ++ cName ++ ".break$" ++ show c ++ "\n"
   in (l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7, c2 + 1)

writeString :: String -> String
writeString [] = ""
writeString (x : xs) = "push constant " ++ show (ord x) ++ "\ncall String.appendChar 2\n" ++ writeString xs

seperateVars :: String -> [AST] -> [AST]
seperateVars _ [] = []
seperateVars exp ((VarDec k t v) : as) =
  if k == exp
    then VarDec k t v : seperateVars exp as
    else seperateVars exp as

writeTable :: Int -> AST -> [Symbol]
writeTable c (VarDec _ _ []) = [Symbol ("", "", "", c)]
writeTable c (VarDec sKind sType (varName : restVarNames)) =
  Symbol (varName, sType, sKind, c) : writeTable (c + 1) (VarDec sKind sType restVarNames)

concatTable :: Int -> [AST] -> [Symbol]
concatTable _ [] = []
concatTable c (x : xs) =
  let firstTable = writeTable c x
      Symbol (_, _, _, c2) = last firstTable
      restTable = concatTable c2 xs
   in init firstTable ++ restTable

paramTable :: Int -> [(String, String)] -> [Symbol]
paramTable _ [] = []
paramTable c ((t, n) : xs) =
  let first = Symbol (n, t, "argument", c)
      rest = paramTable (c + 1) xs
   in first : rest

classTable :: [AST] -> ([Symbol], String)
classTable v =
  let sttT = (concatTable 0 . seperateVars "static" $ v)
      fieT = (concatTable 0 . seperateVars "field" $ v)
      Symbol (_, _, _, fieC) = if null fieT then Symbol ("", "", "", -1) else last fieT
   in (sttT ++ fieT, show (fieC + 1))

subTable :: [Symbol] -> String -> String -> [(String, String)] -> [AST] -> ([Symbol], String)
subTable table sKind className args locals =
  let argT =
        if sKind == "method"
          then paramTable 0 ((className, "this") : args)
          else paramTable 0 args
      lclT = concatTable 0 locals
      Symbol (_, _, _, lclC) = if null lclT then Symbol ("", "", "", -1) else last lclT
   in (argT ++ lclT ++ table, show (lclC + 1))

lookTable :: [Symbol] -> String -> String
lookTable (Symbol (n, t, k, c) : ts) x =
  if n == x
    then case k of
      "static" -> "static " ++ show c ++ "\n"
      "field" -> "this " ++ show c ++ "\n"
      "argument" -> "argument " ++ show c ++ "\n"
      "var" -> "local " ++ show c ++ "\n"
    else lookTable ts x
lookTable table x = error (show table ++ x)

lookTableType :: [Symbol] -> String -> Maybe String
lookTableType (Symbol (n, t, k, c) : ts) x =
  if n == x then Just t else lookTableType ts x
lookTableType _ _ = Nothing

len :: AST -> Int
len (ExpressionList Nothing) = 0
len (ExpressionList (Just x)) = length x
