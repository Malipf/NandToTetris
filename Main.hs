module Main where

import Compiler.Lexer (lexFile)
import Compiler.Parser (astToXML, parseClass)
import Compiler.CodeGenerator (codegenClass)
import VMTranslator.Parser (parseFile)
import VMTranslator.CodeGenerator (FC (..), codegenCommands, codegenInit)
import Assembler.Parser (parseFile)
import Assembler.CodeGenerator (codegenCommands)
import System.Environment (getArgs)
import System.Directory(doesDirectoryExist, listDirectory)
import System.FilePath (replaceExtension, takeBaseName, takeExtension, takeFileName, (</>))
import System.IO (writeFile)
import Control.Monad (foldM)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path_] -> do
      let path = removeTrailingSlash path_
      jackFiles <- getJackFiles path
      osFiles <- getJackFiles "OS"
      vmFiles <- mapM processJackFile (jackFiles ++ osFiles)
      asmFile <- processVMFiles path vmFiles
      let hackLines = unlines . Assembler.CodeGenerator.codegenCommands . Assembler.Parser.parseFile $ snd asmFile
      let hackFile = replaceExtension (fst asmFile) ".hack"
      writeFile hackFile hackLines
      putStrLn "Successfully written."
    _ -> putStrLn "Add folder path as argument"

processJackFile :: FilePath -> IO (String,String)
processJackFile jackFile = do
  tokens <- lexFile jackFile
  let vmLines = codegenClass . parseClass $ tokens
      vmFile = replaceExtension jackFile ".vm"
  return (vmFile, vmLines)

getJackFiles :: FilePath -> IO [FilePath]
getJackFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".jack"]

processVMFiles :: FilePath -> [(String, String)] -> IO (String, String)
processVMFiles dir vmFiles = do
  (asmLinesRev, _) <- foldM step ([], 1) vmFiles
  let asmLines = fst (codegenInit (FC {fileName = "", counter=0})) : reverse asmLinesRev
  let asmFile = dir </> (takeFileName dir ++ ".asm")
  return (asmFile, unlines . concat $ asmLines)
  where
    step (acc, c) (filename,contents) =
      do
        let commands = VMTranslator.Parser.parseFile contents
            linesOut = VMTranslator.CodeGenerator.codegenCommands (FC {fileName = takeBaseName filename, counter=c}) commands
            (codeLines, cLine) = (init linesOut, last linesOut)
            c2 = read cLine
        return (codeLines : acc, c2)

removeTrailingSlash :: FilePath -> FilePath
removeTrailingSlash path
  | not (null path) && last path == '/' = init path
  | otherwise                           = path