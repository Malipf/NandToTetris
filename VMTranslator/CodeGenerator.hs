module VMTranslator.CodeGenerator (FC (..), codegenCommands, codegenInit) where

import VMTranslator.Parser (Command (..), Segment (..))

data FC = FC
  { fileName :: String
  , counter  :: Int
  }

increaseCounter :: FC -> FC
increaseCounter fc = fc { counter = counter fc + 1 }

codegenCommands :: FC -> [Command] -> [String]
codegenCommands fc [] = [show (counter fc)]
codegenCommands fc (x:xs) = 
  let (res, fc2) = codegenCommand fc x
   in res ++ codegenCommands fc2 xs

codegenCommand :: FC -> Command -> ([String], FC)
codegenCommand fc (Pop seg i) = (codegenPop (fileName fc) seg i, fc)
codegenCommand fc (Push seg i) = (codegenPush (fileName fc) seg i, fc)
codegenCommand fc Add = (codegenArithmetic "add", fc)
codegenCommand fc Sub = (codegenArithmetic "sub", fc)
codegenCommand fc Neg = (codegenArithmetic "neg", fc)
codegenCommand fc And = (codegenArithmetic "and", fc)
codegenCommand fc Or = (codegenArithmetic "or", fc)
codegenCommand fc Not = (codegenArithmetic "not", fc)
codegenCommand fc Eq = codegenCompare fc "JEQ"
codegenCommand fc Gt = codegenCompare fc "JGT"
codegenCommand fc Lt = codegenCompare fc "JLT"
codegenCommand fc (Label lName) = (codegenBranch (Label lName), fc)
codegenCommand fc (GoTo lName) = (codegenBranch (GoTo lName), fc)
codegenCommand fc (IfGoTo lName) = (codegenBranch (IfGoTo lName), fc)
codegenCommand fc (Call funcName nArgs) = codegenCall fc funcName nArgs
codegenCommand fc (Function funcName nVars) = (codegenFunction funcName nVars, fc)
codegenCommand fc Return = (codegenReturn, fc)
codegenCommand fc Init = codegenInit fc

toStack :: [String]
toStack = ["@SP","A=M","M=D","@SP","M=M+1"]

fromStack :: [String]
fromStack = ["@SP", "AM=M-1", "D=M"]

codegenArithmetic :: String -> [String]
codegenArithmetic cmd =
  ("// " ++ cmd) : 
  (if cmd == "neg" || cmd == "not"
   then ["@SP", "A=M-1"] ++ (if cmd == "neg" then ["M=-M"] else ["M=!M"])
   else fromStack ++ ["A=A-1"] ++
    case cmd of
       "add" -> ["M=D+M"]
       "sub" -> ["M=M-D"]
       "and" -> ["M=D&M"]
       "or"  -> ["M=D|M"]
  )
  
codegenCompare :: FC -> String -> ([String], FC)
codegenCompare fc jump =
  let trueLabel = "TRUE_" ++ show (counter fc)
      endLabel  = "END_" ++ show (counter fc)
   in ([ "// " ++ jump ,"@SP", "AM=M-1", "D=M", "A=A-1", "D=M-D",
        '@' : trueLabel, "D;" ++ jump,
        "@SP", "A=M-1", "M=0",
        '@' : endLabel, "0;JMP",
        '(' : trueLabel ++ ")", "@SP", "A=M-1", "M=-1",
        '(' : endLabel ++ ")"
       ], increaseCounter fc)

codegenPush :: String -> Segment -> Int -> [String]
codegenPush file seg i = ("// Push " ++  show seg ++ ' ' : show i) :
  case seg of
    Constant -> ['@' : show i, "D=A"]
    Static   -> ['@' : (file ++ ('.' : show i)), "D=M"]
    Temp     -> ['@' : show (5+i), "D=M"]
    Pointer  -> case i of
                  0 -> ['@' : "THIS", "D=M"]
                  1 -> ['@' : "THAT", "D=M"]
    Local    -> pushSeg4 "LCL" i 
    Argument -> pushSeg4 "ARG" i
    This     -> pushSeg4 "THIS" i
    That     -> pushSeg4 "THAT" i
  ++ toStack

pushSeg4 :: String -> Int -> [String]
pushSeg4 seg i = 
  case i of
    0 -> ['@' : seg, "A=M", "D=M"]
    1 -> ['@' : seg, "A=M+1","D=M"]
    2 -> ['@' : seg, "A=M+1","A=A+1", "D=M"]
    _ -> ['@' : seg, "D=M", '@' : show i, "A=D+A", "D=M"]

codegenPop :: String -> Segment -> Int -> [String]
codegenPop file seg i = ("// Pop " ++ show seg ++ ' ' : show i) :
  case seg of
    Constant -> ["// pop constant not allowed"]
    Static   -> fromStack ++ ['@' : (file ++ ('.' : show i)), "M=D"]
    Temp     -> fromStack ++ ['@' : show (5+i), "M=D"]
    Pointer  -> case i of
                  0 -> fromStack ++ ['@' : "THIS", "M=D"]
                  1 -> fromStack ++ ['@' : "THAT", "M=D"]
    Local    -> popSeg4 "LCL" i
    Argument -> popSeg4 "ARG" i
    This     -> popSeg4 "THIS" i
    That     -> popSeg4 "THAT" i

popSeg4 :: String -> Int -> [String]
popSeg4 seg i  
  | i == 0 = fromStack ++ ['@' : seg, "A=M", "M=D"]
  | i <= 6 = fromStack ++ ['@' : seg, "A=M+1"] ++
             replicate (i - 1) "A=A+1" ++ ["M=D"] 
  | i > 6  = ['@' : seg, "D=M", '@' : show i, "D=D+A",
              "@R13", "M=D"] ++ fromStack ++ ["@R13", "A=M", "M=D"]

codegenBranch :: Command -> [String]
codegenBranch command =
  case command of
    (Label lName)  -> ["// Label " ++ lName, '(' : lName ++ ")"]
    (GoTo lName)   -> ["// go to " ++ lName, '@' : lName, "0;JMP"]
    (IfGoTo lName) -> ("// if go to " ++ lName) : fromStack ++ ['@' : lName, "D;JNE"]

codegenCall :: FC -> String -> Int -> ([String], FC)
codegenCall fc func n = 
    let retLabel = func  ++ "$ret." ++ show (counter fc)
     in (("// Call " ++ func ++ " " ++ show n) :
         [ '@' : retLabel, "D=A"] ++ toStack ++
         ["@LCL", "D=M"] ++ toStack ++
         ["@ARG", "D=M"] ++ toStack ++
         ["@THIS", "D=M"] ++ toStack ++
         ["@THAT", "D=M"] ++ toStack ++
         ["@SP", "D=M", '@' : show (n+5), "D=D-A", "@ARG", "M=D",
          "@SP", "D=M", "@LCL", "M=D",
          '@' : func, "0;JMP", '(' : retLabel ++ ")"], increaseCounter fc)
      
codegenFunction :: String -> Int -> [String]
codegenFunction func n = ["// Function " ++ func ++ " " ++ show n,
                         '(' : func ++ ")"] ++
                         concat (replicate n (["@0", "D=A"] ++ toStack))

codegenReturn :: [String]
codegenReturn =
  [ "// return"
  , "@LCL", "D=M", "@R13", "M=D"
  , "@5", "A=D-A", "D=M", "@R14", "M=D"    
  , "@SP", "A=M-1", "D=M", "@ARG", "A=M", "M=D"
  , "@ARG", "D=M+1", "@SP", "M=D"
  , "@R13", "AM=M-1", "D=M", "@THAT", "M=D"
  , "@R13", "AM=M-1", "D=M", "@THIS", "M=D"
  , "@R13", "AM=M-1", "D=M", "@ARG", "M=D"
  , "@R13", "AM=M-1", "D=M", "@LCL", "M=D"
  , "@R14", "A=M", "0;JMP"
  ]

codegenInit :: FC -> ([String], FC)
codegenInit fc = (["// Bootstrap code", "@256", "D=A", "@SP", "M=D"] 
                     ++ fst (codegenCall fc "Sys.init" 0), increaseCounter fc)

