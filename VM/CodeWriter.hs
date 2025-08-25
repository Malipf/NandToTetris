module CodeWriter (writeFileCommands) where

import Parser (Command (..), Segment (..))
import System.FilePath (takeBaseName)
import Control.Monad.State

writeFileCommands :: FilePath -> [Command] -> [String]
writeFileCommands fileName commands =
  evalState (fmap concat $ mapM (writeCommand . takeBaseName $ fileName) commands) 0

writeCommand :: String -> Command -> State Int [String]
writeCommand file (Pop seg i)  = return $ writePop file seg i
writeCommand file (Push seg i) = return $ writePush file seg i
writeCommand _ Add             = return $ writeArithmetic "add"
writeCommand _ Sub             = return $ writeArithmetic "sub"
writeCommand _ Neg             = return $ writeArithmetic "neg"
writeCommand _ And             = return $ writeArithmetic "and"
writeCommand _ Or              = return $ writeArithmetic "or"
writeCommand _ Not             = return $ writeArithmetic "not"
writeCommand _ Eq              = writeCompare "JEQ"
writeCommand _ Gt              = writeCompare "JGT"
writeCommand _ Lt              = writeCompare "JLT"
writeCommand _ (Label lName)   = return $ writeBranch (Label lName)
writeCommand _ (GoTo lName)    = return $ writeBranch (GoTo lName)
writeCommand _ (IfGoTo lName)  = return $ writeBranch (IfGoTo lName)
writeCommand _ (Call funcName nArgs)     = writeCall funcName nArgs
writeCommand _ (Function funcName nVars) = return (writeFunction funcName nVars)
writeCommand _ Return                    = return writeReturn
writeCommand _ Init                      = writeInit

toStack :: [String]
toStack = ["@SP","A=M","M=D","@SP","M=M+1"]

fromStack :: [String]
fromStack = ["@SP", "AM=M-1", "D=M"]

writeArithmetic :: String -> [String]
writeArithmetic cmd =
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
  
writeCompare :: String -> State Int [String]
writeCompare jump =
  do
    c <- get
    modify (+1)
    let trueLabel = "TRUE_" ++ show c
    let endLabel  = "END_" ++ show c
    return
      [ "// " ++ jump ,"@SP", "AM=M-1", "D=M", "A=A-1", "D=M-D",
        '@' : trueLabel, "D;" ++ jump,
        "@SP", "A=M-1", "M=0",
        '@' : endLabel, "0;JMP",
        '(' : trueLabel ++ ")", "@SP", "A=M-1", "M=-1",
        '(' : endLabel ++ ")"
      ]

writePush :: String -> Segment -> Int -> [String]
writePush file seg i = ("// Push " ++  show seg ++ ' ' : show i) :
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

writePop :: String -> Segment -> Int -> [String]
writePop file seg i = ("// Pop " ++ show seg ++ ' ' : show i) :
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

writeBranch :: Command -> [String]
writeBranch command =
  case command of
    (Label lName)  -> ["// Label " ++ lName, '(' : lName ++ ")"]
    (GoTo lName)   -> ["// go to " ++ lName, '@' : lName, "0;JMP"]
    (IfGoTo lName) -> ("// if go to " ++ lName) : fromStack ++ ['@' : lName, "D;JNE"]

writeCall :: String -> Int -> State Int [String]
writeCall func n = 
  do
    c <- get
    modify (+1)
    let retLabel = func  ++ "$ret." ++ show c
    return
      (("// Call " ++ func ++ " " ++ show n) :
       [ '@' : retLabel, "D=A"] ++ toStack ++
       ["@LCL", "D=M"] ++ toStack ++
       ["@ARG", "D=M"] ++ toStack ++
       ["@THIS", "D=M"] ++ toStack ++
       ["@THAT", "D=M"] ++ toStack ++
       ["@SP", "D=M", '@' : show (n+5), "D=D-A", "@ARG", "M=D",
        "@SP", "D=M", "@LCL", "M=D",
        '@' : func, "0;JMP", '(' : retLabel ++ ")"])
      
writeFunction :: String -> Int -> [String]
writeFunction func n = ["// Function " ++ func ++ " " ++ show n,
                        '(' : func ++ ")"] ++
                       concat ( replicate n (["@0", "D=A"] ++ toStack))

writeReturn :: [String]
writeReturn =
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

writeInit :: State Int [String]
writeInit = do
  callSys <- writeCall "Sys.init" 0
  return ( ["// Bootstrap code", "@256", "D=A", "@SP", "M=D"] ++ callSys )

