module Compiler.Main where

import Compiler.Lexer (lexFile)
import Compiler.Parser (astToXML, parseClass)
import Compiler.CodeGenerator (codegenClass)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeBaseName, takeExtension, takeFileName, (</>))
import System.IO (writeFile)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      jackFiles <- getJackFiles path
      mapM_ processJackFile jackFiles
      putStrLn "Successfully written."
    _ -> putStrLn "Error"

processJackFile :: FilePath -> IO ()
processJackFile jackFile = do
  tokens <- lexFile jackFile
  let xmlLines = astToXML . parseClass $ tokens
  let xmlFile = replaceExtension jackFile ".xml"
  let vmLines = codegenClass . parseClass $ tokens
  let vmFile = replaceExtension jackFile ".vm"
  writeFile vmFile vmLines
  writeFile xmlFile xmlLines

getJackFiles :: FilePath -> IO [FilePath]
getJackFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".jack"]
