module JackAnalyzer where

import CompilationEngine (astToXML, compileClass)
import JackTokenizer (parseFile)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeBaseName, takeExtension, takeFileName, (</>))
import System.IO (writeFile)
import VMWriter

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
  let xmlFile = replaceExtension jackFile ".xml"
  let vmLines = writeClass . compileClass $ tokens
  let vmFile = replaceExtension jackFile ".vm"
  writeFile vmFile vmLines
  writeFile xmlFile xmlLines

getJackFiles :: FilePath -> IO [FilePath]
getJackFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".jack"]
