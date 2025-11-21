module VMTranslator.Main where

import VMTranslator.Parser (parseFile)
import VMTranslator.CodeGenerator (FC (..), codegenCommands, codegenInit)
import System.IO (writeFile)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, takeExtension, (</>), takeBaseName, replaceExtension)
import Control.Monad (foldM)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path_] -> do
      let path = removeTrailingSlash path_
      vmFiles <- getVMFiles path
      processVMFiles path vmFiles
      putStrLn "Successfully written."
    _ -> error "Error"

processVMFiles :: FilePath -> [FilePath] -> IO ()
processVMFiles dir vmFiles = do
  (asmLinesRev, _) <- foldM step ([], 1) vmFiles
  let asmLines = fst (codegenInit (FC {fileName = "", counter=0})) : reverse asmLinesRev
  let outFile = dir </> (takeFileName dir ++ ".asm")
  writeFile outFile (unlines . concat $ asmLines)
  where
    step (acc, c) filename =
      do
        contents <- readFile filename
        let commands = parseFile contents
            linesOut = codegenCommands (FC {fileName = takeBaseName filename, counter=c}) commands
            (codeLines, cLine) = (init linesOut, last linesOut)
            c2 = read cLine
        return (codeLines : acc, c2)

getVMFiles :: FilePath -> IO [FilePath]
getVMFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".vm"]

removeTrailingSlash :: FilePath -> FilePath
removeTrailingSlash path
  | not (null path) && last path == '/' = init path
  | otherwise                           = path
