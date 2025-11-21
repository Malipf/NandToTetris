module Assembler.CodeGenerator where

import Assembler.Parser ( Instruction(..), filterNull )

data Symbol = Symbol String Int
  deriving (Show, Eq)

builtins :: [Symbol]
builtins = 
  [ Symbol "R0" 0
  , Symbol "R1" 1
  , Symbol "R2" 2
  , Symbol "R3" 3
  , Symbol "R4" 4
  , Symbol "R5" 5
  , Symbol "R6" 6
  , Symbol "R7" 7
  , Symbol "R8" 8
  , Symbol "R9" 9
  , Symbol "R10" 10
  , Symbol "R11" 11
  , Symbol "R12" 12
  , Symbol "R13" 13
  , Symbol "R14" 14
  , Symbol "R15" 15
  , Symbol "SCREEN" 16384
  , Symbol "KBD" 24576
  , Symbol "SP" 0
  , Symbol "LCL" 1
  , Symbol "ARG" 2
  , Symbol "THIS" 3
  , Symbol "THAT" 4
  ]
  
lookTable ::  [Symbol] -> String -> Maybe Int
lookTable [] _ = Nothing
lookTable ((Symbol k v):xs) key = if key == k then Just v else lookTable xs key

addLabelSymbols :: [Symbol] -> [Instruction] -> [Symbol]
addLabelSymbols table [] = table
addLabelSymbols table ((Label lName value) : xs) = Symbol lName value : addLabelSymbols table xs
addLabelSymbols table (x:xs) = addLabelSymbols table xs

addVarSymbols :: Int -> [Instruction] -> [Symbol] -> [Symbol]
addVarSymbols _ [] table = table
addVarSymbols c ((AS vName) : xs) table = 
  if lookTable table vName == Nothing 
    then Symbol vName c : addVarSymbols (c+1) xs table
    else addVarSymbols c xs table
addVarSymbols c (x:xs) table = addVarSymbols c xs table

codegenCommands :: [Instruction] -> [String]
codegenCommands ins = 
  let table = addVarSymbols 16 ins . addLabelSymbols builtins $ ins
   in filterNull . map (codegenCommand table) $ ins

codegenCommand :: [Symbol] -> Instruction -> String
codegenCommand _ (AN addr) = toBin16 (toBin addr)
codegenCommand table (AS symbol) = toBin16 . toBin . (\(Just x) -> x) . lookTable table $ symbol
codegenCommand _ (C comp dest jump) = "111" ++ codegenComp comp ++ codegenDest dest ++ codegenJump jump
codegenCommand _ _ = ""

toBin :: Int -> String
toBin 0 = "0"
toBin 1 = "1"
toBin x = toBin (x `div` 2) ++ show (x `mod` 2)

toBin16 :: String -> String
toBin16 x = '0' : replicate_ (15 - length x) '0' ++ x

replicate_ :: Int -> Char -> String
replicate_ x 
  | x < 0 = error "Hack file bigger than hack rom."
  | otherwise = replicate x

codegenJump :: String -> String
codegenJump x =
  case x of
    ""    -> "000"
    "JGT" -> "001"
    "JEQ" -> "010"
    "JGE" -> "011"
    "JLT" -> "100"
    "JNE" -> "101"
    "JLE" -> "110"
    "JMP" -> "111"

codegenDest :: String -> String
codegenDest x = has 'A' x ++ has 'D' x ++ has 'M' x
  where 
    has c s = if c `elem` s then "1" else "0"

codegenComp :: String -> String
codegenComp x = 
  case x of 
    "0"   -> "0101010"
    "1"   -> "0111111"
    "-1"  -> "0111010"
    "D"   -> "0001100"
    "A"   -> "0110000"
    "M"   -> "1110000"
    "!D"  -> "0001101"
    "!A"  -> "0110001"
    "!M"  -> "1110001"
    "-D"  -> "0001111"
    "-A"  -> "0110011"
    "-M"  -> "1110011"
    "D+1" -> "0011111"
    "A+1" -> "0110111"
    "M+1" -> "1110111"
    "D-1" -> "0001110"
    "A-1" -> "0110010"
    "M-1" -> "1110010"
    "D+A" -> "0000010"
    "A+D" -> "0000010"
    "D+M" -> "1000010"
    "M+D" -> "1000010"
    "D-A" -> "0010011"
    "D-M" -> "1010011"
    "A-D" -> "0000111"
    "M-D" -> "1000111"
    "D&A" -> "0000000"
    "A&D" -> "0000000"
    "D&M" -> "1000000"
    "M&D" -> "1000000"
    "D|A" -> "0010101"
    "A|D" -> "0010101"
    "D|M" -> "1010101"
    "M|D" -> "1010101"

    
