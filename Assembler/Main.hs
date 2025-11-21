module Assembler.Main where

import Assembler.Parser ( parseFile )
import Assembler.CodeGenerator ( codegenCommands )
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import System.FilePath (replaceExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      content <- readFile path
      let hackLines = unlines . codegenCommands . parseFile $ content
      let outFile = replaceExtension path ".hack"
      writeFile outFile hackLines
      putStrLn "Successfully written."
    _ -> error "Error"
