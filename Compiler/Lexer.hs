module Compiler.Lexer where

data Token
  = Keyword Keyword
  | Symbol Symbol
  | Identifier String
  | IntVal Int
  | StrVal String
  deriving (Show, Eq)

data Keyword
  = CLASS | METHOD | FUNCTION | CONSTRUCTOR 
  | INT | BOOLEAN | CHAR | VOID
  | VAR | STATIC | FIELD 
  | LET | DO | IF | ELSE | WHILE | RETURN
  | TRUE | FALSE | NULL | THIS
  deriving (Show, Eq)

data Symbol
  = LCB | RCB | LP | RP | LB | RB
  | POINT | COMMA | SEMICOLON 
  | PLUS | MINUS | ASTERISK
  | SLASH | AMPERSAND | BAR
  | LESS_THAN | GREATER_THAN
  | EQUAL | TILDE
  deriving (Show, Eq)

lexFile :: FilePath -> IO [Token]
lexFile path = do
  contents <- readFile path
  return . map lexTokens . concatMap words_ . sus . seperateStrings . seperateSymbols . stripComments $ contents

lexTokens :: String -> Token
lexTokens token =
  case token of
    "class" -> Keyword CLASS
    "method" -> Keyword METHOD
    "function" -> Keyword FUNCTION
    "constructor" -> Keyword CONSTRUCTOR
    "int" -> Keyword INT
    "boolean" -> Keyword BOOLEAN
    "char" -> Keyword CHAR
    "void" -> Keyword VOID
    "var" -> Keyword VAR
    "static" -> Keyword STATIC
    "field" -> Keyword FIELD
    "let" -> Keyword LET
    "do" -> Keyword DO
    "if" -> Keyword IF
    "else" -> Keyword ELSE
    "while" -> Keyword WHILE
    "return" -> Keyword RETURN
    "true" -> Keyword TRUE
    "false" -> Keyword FALSE
    "null" -> Keyword NULL
    "this" -> Keyword THIS

    "{" -> Symbol LCB
    "}" -> Symbol RCB
    "(" -> Symbol LP
    ")" -> Symbol RP
    "[" -> Symbol LB
    "]" -> Symbol RB
    "." -> Symbol POINT
    "," -> Symbol COMMA
    ";" -> Symbol SEMICOLON
    "+" -> Symbol PLUS
    "-" -> Symbol MINUS
    "*" -> Symbol ASTERISK
    "/" -> Symbol SLASH
    "&" -> Symbol AMPERSAND
    "|" -> Symbol BAR
    "<" -> Symbol LESS_THAN
    ">" -> Symbol GREATER_THAN
    "=" -> Symbol EQUAL
    "~" -> Symbol TILDE
    
    _ | all (`elem` ['0' .. '9']) token -> IntVal (read token)
    _ | head token == '"' && last token == '"' -> StrVal (init . tail $ token)
    _ -> Identifier token

seperateStrings :: String -> String
seperateStrings [] = []
seperateStrings ('"' : xs) = '"' : seperateStrings2 xs
seperateStrings (x : xs) = x : seperateStrings xs

seperateStrings2 :: String -> String
seperateStrings2 [] = []
seperateStrings2 ('ඞ' : xs) = seperateStrings2 xs
seperateStrings2 ('"' : xs) = '"' : seperateStrings xs
seperateStrings2 (x : xs) = x : seperateStrings2 xs

seperateSymbols :: String -> String
seperateSymbols [] = []
seperateSymbols (x : xs)
  | x `elem` "{}()[].,;+-*/&|<>=~\"" = 'ඞ' : x : 'ඞ' : seperateSymbols xs
  | otherwise = x : seperateSymbols xs

words_ :: String -> [String]
words_ s =
  if head s /= '"'
    then words s
    else [s]

sus :: String -> [String] 
sus s =
  case dropWhile (== 'ඞ') s of
    "" -> []
    s' -> w : sus s''
      where
        (w, s'') = break (== 'ඞ') s'

stripComments :: String -> String
stripComments [] = []
stripComments ('/' : '/' : xs) = inLine xs
stripComments ('/' : '*' : xs) = inMulti xs
stripComments (x : xs) = x : stripComments xs

inLine :: String -> String
inLine [] = []
inLine ('\n' : xs) = stripComments xs
inLine (_ : xs) = inLine xs

inMulti :: String -> String
inMulti [] = []
inMulti ('*' : '/' : xs) = stripComments xs
inMulti (_ : xs) = inMulti xs
