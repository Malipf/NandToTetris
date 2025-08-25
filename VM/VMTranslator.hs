module VMTranslator where

import qualified Parser (parseFile)
import qualified CodeWriter (writeFileCommands)
import System.IO (writeFile)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (takeFileName, takeExtension, (</>), takeBaseName, replaceExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path_] -> do
      let path = removeTrailingSlash path_
      isDir <- doesDirectoryExist path
      vmFiles <- if isDir then getVMFiles path else return [path]

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
  let initFile = dir </> "InitFileTemp.vm"
  writeFile initFile "init\n"
  let allVMFiles = initFile : vmFiles
  allAsmLines <- fmap concat $ mapM (\fileName -> do
      commands <- Parser.parseFile fileName
      return (CodeWriter.writeFileCommands fileName commands)) allVMFiles

  let outFile = dir </> (takeFileName dir ++ ".asm")
  writeFile outFile (unlines allAsmLines)
  removeFile initFile

getVMFiles :: FilePath -> IO [FilePath]
getVMFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".vm"]

removeTrailingSlash :: FilePath -> FilePath
removeTrailingSlash path
  | not (null path) && last path == '/' = init path
  | otherwise                           = path
