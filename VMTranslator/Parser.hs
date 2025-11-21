module VMTranslator.Parser (Command(..), Segment(..), parseFile)  where

data Segment = Argument | Local | Static | Constant | This | That | Pointer | Temp
  deriving (Show, Eq)

data Command
  = Push Segment Int
  | Pop Segment Int
  | Add | Sub | Neg
  | Eq | Gt | Lt
  | And | Or | Not
  | Label String
  | GoTo String
  | IfGoTo String
  | Call String Int
  | Function String Int
  | Return | Init
  deriving (Show, Eq)

parseFile :: String -> [Command]
parseFile = map parseCommand . filterNull . map cleanLines . lines

cleanLines :: String -> String
cleanLines = dropWhile (== '\t') . takeWhile (/= '/') . dropWhile (== ' ')

filterNull :: [String] -> [String]
filterNull = filter (not . null)

parseCommand :: String -> Command
parseCommand line =
  case words line of
    ["push", seg, n] -> Push (parseSegment seg) (read n)
    ["pop", seg, n]  -> Pop  (parseSegment seg) (read n)
    ["add"] -> Add
    ["sub"] -> Sub
    ["neg"] -> Neg
    ["eq"]  -> Eq
    ["gt"]  -> Gt
    ["lt"]  -> Lt
    ["and"] -> And
    ["or"]  -> Or
    ["not"] -> Not
    ["label", lName]   -> Label lName
    ["goto", lName]    -> GoTo lName
    ["if-goto", lName] -> IfGoTo lName
    ["call", funcName, nArgs]     -> Call funcName (read nArgs)
    ["function", funcName, nVars] -> Function funcName (read nVars)
    ["return"]                    -> Return
    ["init"]                      -> Init

parseSegment :: String -> Segment
parseSegment s =
  case s of
    "argument" -> Argument
    "local"    -> Local
    "static"   -> Static
    "constant" -> Constant
    "this"     -> This
    "that"     -> That
    "pointer"  -> Pointer
    "temp"     -> Temp

