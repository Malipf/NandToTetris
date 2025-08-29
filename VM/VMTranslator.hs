module VMTranslator where

import qualified Parser (parseFile, Command(Init))
import qualified CodeWriter (writeFileCommands)
import System.IO (writeFile)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, takeExtension, (</>), takeBaseName, replaceExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path_] -> do
      let path = removeTrailingSlash path_
      isDir <- doesDirectoryExist path
      if isDir
        then do
          vmFiles <- getVMFiles path
          if null vmFiles
            then putStrLn "No .vm files found."
            else processVMFiles path vmFiles
        else
          processVMFile path

      putStrLn "Successfully written."
    _ -> putStrLn "Usage: VMTranslator.hs <inputFile.vm> or <inputDirectory>"

processVMFile :: FilePath -> IO ()
processVMFile vmFile = do
  commands <- Parser.parseFile vmFile
  let asmLines = CodeWriter.writeFileCommands vmFile commands
  let outFile = replaceExtension vmFile ".asm"
  writeFile outFile (unlines asmLines)

processVMFiles :: FilePath -> [FilePath] -> IO ()
processVMFiles dir vmFiles = do
  let init_ = CodeWriter.writeFileCommands (head vmFiles) [Parser.Init]
  asmLines <- fmap concat $ mapM (\fileName -> do
      commands <- Parser.parseFile fileName
      return (CodeWriter.writeFileCommands fileName commands)) vmFiles

  let allAsmLines =  init_ ++ asmLines
  let outFile = dir </> (takeFileName dir ++ ".asm")
  writeFile outFile (unlines allAsmLines)

getVMFiles :: FilePath -> IO [FilePath]
getVMFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".vm"]

removeTrailingSlash :: FilePath -> FilePath
removeTrailingSlash path
  | not (null path) && last path == '/' = init path
  | otherwise                           = path
