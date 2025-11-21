module Assembler.Parser where

data Instruction 
  = AS String
  | AN Int
  | C String String String
  | Label String Int
  deriving (Show, Eq)

parseFile :: String -> [Instruction]
parseFile = map fst . giveLineNumbers 0 . map parseInstruction . filterNull . map cleanLines . lines

parseInstruction :: String -> Instruction
parseInstruction ('@':addr) = if allDigit addr then AN (read addr) else AS addr
parseInstruction ('(':label) = Label (init label) 0
parseInstruction line = C (seperateComp line) (seperateDest line) (seperateJumps line)

allDigit :: String -> Bool
allDigit "" = True
allDigit (x:xs) = x `elem` "0123456789" && allDigit xs

giveLineNumbers :: Int -> [Instruction] -> [(Instruction, Int)]
giveLineNumbers _ [] = []
giveLineNumbers c ((Label lb _):xs) = (Label lb c, c) : giveLineNumbers c xs
giveLineNumbers c (x:xs) = (x, c) : giveLineNumbers (c+1) xs

cleanLines :: String -> String
cleanLines = dropWhile (== '\t') . takeWhile (/= ' ') . takeWhile (/= '/') . dropWhile (== ' ')

filterNull :: [String] -> [String]
filterNull = filter (not . null)

seperateJumps :: String -> String
seperateJumps = safeTail . dropWhile (/= ';')

seperateDest :: String -> String
seperateDest = reverse . safeTail . dropWhile (/= '=') . reverse . takeWhile (/= ';')

seperateComp :: String -> String 
seperateComp = reverse . takeWhile (/= '=') . reverse . takeWhile (/= ';')

safeTail :: String -> String
safeTail "" = ""
safeTail (x:xs) = xs