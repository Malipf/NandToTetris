module Compiler.CodeGenerator where

import Compiler.Parser ( AST(..) )
import Data.Char (ord)

newtype Symbol = Symbol (String, String, String, Int)
  deriving (Eq, Show)

data GlobalContainer = GC {
  className :: String,
  counterStatements :: Int,
  counterFields :: String,
  table :: [Symbol]
} 

increaseStatementCounter :: GlobalContainer -> GlobalContainer
increaseStatementCounter gc = gc {counterStatements = counterStatements gc + 1}

updateTable :: GlobalContainer -> [Symbol] -> GlobalContainer
updateTable gc newTable = gc {table = newTable}

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

codegenClass :: AST -> String
codegenClass (Class name varDec subDec) =
  let (cTable, cField) = classTable varDec
      gc = GC {className = name, counterStatements = 0, 
               counterFields = cField, table = cTable}
      (subs, _) = codegenSubs gc subDec
   in subs

codegenSubs :: GlobalContainer -> [AST] -> (String, GlobalContainer)
codegenSubs gc [] = ("", gc)
codegenSubs gc (x : xs) =
  let (first, gc2) = codegenSub gc x
      (rest, gc3) = codegenSubs gc2 xs
   in (first ++ rest, gc3)

codegenSub :: GlobalContainer -> AST -> (String, GlobalContainer)
codegenSub gc (Subroutine sKind sType sName params (SubroutineBody varDec statements)) =
  let (newTable, counterLocals) = subTable (table gc) sKind (className gc) params varDec
      func               = "function " ++ className gc ++ "." ++ sName ++ " " ++ counterLocals ++ "\n"
      kindExtras =
        case sKind of
          "constructor" -> "push constant " ++ counterFields gc ++ "\n" ++ 
                           "call Memory.alloc 1\n\
                           \pop pointer 0\n"
          
          "method"      -> "push argument 0\n\
                           \pop pointer 0\n"
          
          "function"    -> ""
      (stmnts, gc2) = codegenStatements (updateTable gc newTable) statements
   in (func ++ kindExtras ++ stmnts, gc2)

codegenExpression :: GlobalContainer -> AST -> String
codegenExpression gc (Expression term []) = codegenTerm gc term

codegenExpression gc (Expression firstTerm rest) =
  codegenTerm gc firstTerm ++ 
  concatMap (\(op, term) -> codegenTerm gc term ++ codegenOp op) rest

codegenExpression _ (ExpressionList Nothing) = ""

codegenExpression gc (ExpressionList (Just expressions)) =
  concatMap (codegenExpression gc) expressions

codegenTerm :: GlobalContainer -> AST -> String
codegenTerm _ (TermInt i)          = "push constant " ++ show i ++ "\n"

codegenTerm _ (TermString s)       = "push constant " ++ show (length s) ++ "\n\
                                     \call String.new 1\n" ++ 
                                      codegenString s

codegenTerm _ (TermConstant x)     =  codegenConstant x

codegenTerm gc (TermVar x)         = "push " ++ lookTable (table gc) x

codegenTerm gc (TermArr name expr) = "push " ++ lookTable (table gc) name ++ "\n" ++ 
                                      codegenExpression gc expr ++ 
                                      "add\n\
                                      \pop pointer 1\n\
                                      \push that 0\n"

codegenTerm gc (TermCall (ThisMethodCall func expr)) =
  let pushThis = "push pointer 0\n"
      paramCounter = show (len expr + 1)
      rest =  codegenExpression gc expr ++ 
             "call " ++ className gc ++ "." ++ func ++ " " ++ paramCounter ++ "\n"
   in pushThis ++ rest

codegenTerm gc (TermCall (SubroutineCall caller func expr)) =
  let callerType = lookTableType (table gc) caller
      isCons = callerType == Nothing
      caller2 = if isCons then Just caller else callerType
      (Just caller3) = caller2
      paramLength = if isCons then show (len expr) else show (len expr + 1)
      pushThis = if isCons then "" else "push " ++ lookTable (table gc) caller
   in pushThis ++ codegenExpression gc expr ++ 
      "call " ++ caller3 ++ "." ++ func ++ " " ++ paramLength ++ "\n"

codegenTerm gc (TermExpr expr) = codegenExpression gc expr

codegenTerm gc (TermOp op term) = codegenTerm gc term ++ codegenUnOp op

len :: AST -> Int
len (ExpressionList Nothing) = 0
len (ExpressionList (Just x)) = length x

codegenConstant :: String -> String
codegenConstant x =
  case x of
    "true"  -> "push constant 1\nneg\n"
    "false" -> "push constant 0\n"
    "this"  -> "push pointer 0\n"
    "null"  -> "push constant 0\n"

codegenOp :: String -> String
codegenOp x =
  case x of
    "plus"         -> "add\n"
    "minus"        -> "sub\n"
    "asterisk"     -> "call Math.multiply 2\n"
    "slash"        -> "call Math.divide 2\n"
    "ampersand"    -> "and\n"
    "bar"          -> "or\n"
    "less_than"    -> "lt\n"
    "greater_than" -> "gt\n"
    "equal"        -> "eq\n"

codegenUnOp :: String -> String
codegenUnOp "tilde" = "not\n"
codegenUnOp "minus" = "neg\n"

lookTable :: [Symbol] -> String -> String
lookTable (Symbol (n, t, k, c) : ts) x =
  if n == x
    then case k of
      "static"   -> "static " ++ show c ++ "\n"
      "field"    -> "this " ++ show c ++ "\n"
      "argument" -> "argument " ++ show c ++ "\n"
      "var"      -> "local " ++ show c ++ "\n"
    else lookTable ts x
lookTable table x = error (show table ++ x)

lookTableType :: [Symbol] -> String -> Maybe String
lookTableType (Symbol (n, t, k, c) : ts) x =
  if n == x then Just t else lookTableType ts x
lookTableType _ _ = Nothing

codegenStatements :: GlobalContainer -> [AST] -> (String, GlobalContainer)
codegenStatements gc [] = ("", gc)
codegenStatements gc (x : xs) =
  let (first, gc2) = codegenStatement gc x
      (rest, gc3) = codegenStatements gc2 xs
   in (first ++ rest, gc3)

codegenStatement :: GlobalContainer -> AST -> (String, GlobalContainer)
codegenStatement gc (LetStatement vName Nothing expression) =
  (codegenExpression gc expression ++ "pop " ++ lookTable (table gc) vName, gc)
codegenStatement gc (LetStatement name (Just iex) oex) =
  ( "push " ++ lookTable (table gc) name ++ 
     codegenExpression gc iex ++ 
    "add\n" ++ 
     codegenExpression gc oex ++ 
    "pop temp 0\n\
    \pop pointer 1\n\
    \push temp 0\n\
    \pop that 0\n"
    , gc
  )

codegenStatement gc (ReturnStatement Nothing) = 
  ("push constant 0\nreturn\n", gc)

codegenStatement gc (ReturnStatement (Just expr)) = 
  (codegenExpression gc expr ++ "return\n", gc)

codegenStatement gc (DoStatement expr) =
  (codegenExpression gc (Expression (TermCall expr) [])
   ++ "pop temp 0\n", gc)

codegenStatement gc (IfStatement expression ifStatement Nothing) =
  let l1 = codegenExpression gc expression
      l2 = "not\n"
      l3 = "if-goto " ++ className gc ++ ".else$" ++ show (counterStatements gc) ++ "\n"
      (l4, gc2) = codegenStatements (increaseStatementCounter gc) ifStatement
      l5 = "goto " ++ className gc ++ ".if$" ++ show (counterStatements gc) ++ "\n"
      l6 = "label " ++ className gc ++ ".else$" ++ show (counterStatements gc) ++ "\n"
      l7 = "label " ++ className gc ++ ".if$" ++ show (counterStatements gc) ++ "\n"
   in (l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7, increaseStatementCounter gc2)

codegenStatement gc (IfStatement expression ifStatement (Just elseStatement)) =
  let l1 = codegenExpression gc expression
      l2 = "not\n"
      l3 = "if-goto " ++ className gc ++ ".else$" ++ show (counterStatements gc) ++ "\n"
      (l4, gc2) = codegenStatements (increaseStatementCounter gc) ifStatement
      l5 = "goto " ++ className gc ++ ".if$" ++ show (counterStatements gc) ++ "\n"
      l6 = "label " ++ className gc ++ ".else$" ++ show (counterStatements gc) ++ "\n"
      (l7, gc3) = codegenStatements gc2 elseStatement
      l8 = "label " ++ className gc ++ ".if$" ++ show (counterStatements gc) ++ "\n"
   in (l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7 ++ l8, increaseStatementCounter gc3)

codegenStatement gc (WhileStatement expression statements) =
  let l1 = "label " ++ className gc ++ ".loop$" ++ show (counterStatements gc) ++ "\n"
      l2 = codegenExpression gc expression
      l3 = "not\n"
      l4 = "if-goto " ++ className gc ++ ".break$" ++ show (counterStatements gc) ++ "\n"
      (l5, gc2) = codegenStatements (increaseStatementCounter gc) statements
      l6 = "goto " ++ className gc ++ ".loop$" ++ show (counterStatements gc) ++ "\n"
      l7 = "label " ++ className gc ++ ".break$" ++ show (counterStatements gc) ++ "\n"
   in (l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7, increaseStatementCounter gc2)

codegenString :: String -> String
codegenString [] = ""
codegenString (x : xs) = "push constant " ++ show (ord x) ++ "\n\
                         \call String.appendChar 2\n" ++ 
                          codegenString xs
