module JackAnalyzer where

import JackTokenizer (parseFile)
import CompilationEngine (compileClass, astToXML)
import System.IO (writeFile)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, takeExtension, (</>), takeBaseName, replaceExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          jackFiles <- getJackFiles path
          mapM_ processJackFile jackFiles
        else
          processJackFile path

      putStrLn "Successfully written."
    _ -> putStrLn "Usage: JackAnalyzer.hs <inputFile.jack> or <inputDirectory>"

processJackFile :: FilePath -> IO ()
processJackFile jackFile = do
  tokens <- parseFile jackFile
  let xmlLines = astToXML . compileClass $ tokens
  let outFile = replaceExtension jackFile ".xml"
  writeFile outFile xmlLines

getJackFiles :: FilePath -> IO [FilePath]
getJackFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".jack"]

