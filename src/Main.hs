-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Description :  Copies all files with the specified filename beginning
--                from the first directory to the second directory
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
-- Maintainer  :  artemtsushko@gmail.com
-- Stability   :  provisional
-- Portability :  POSIX
--
-- Copies all files with the specified filename beginning
-- from the first directory to the second directory
-- /Usage/
-- Provide the input directory and the output directory as command line args.
-- Then enter a filename beginning pattern.
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.Directory
import System.Environment
import System.FilePath.Posix (combine)
import System.IO
import System.IO.Error
import System.Posix
import Control.Monad
import Data.List

main :: IO ()
main = copyMatchingFiles `catchIOError`  handleError

{- | Asks user for filename beginning and then copies all files matching this
     filename pattern from the first directory to the second one. Directory
     paths are provided as command line arguments
-}
copyMatchingFiles :: IO ()
copyMatchingFiles = do
    (inputDir:outputDir:_) <- mapM canonicalizePath =<< getArgs
    if inputDir /= outputDir
    then do
        beginning <- putStr "enter file beginning: " >> hFlush stdout >> getLine
        filesToCopy <- filterM (doesFileExist . combine inputDir)
                     . filter (beginning `isPrefixOf`)
                   =<< getDirectoryContents inputDir
        sizes <- forM filesToCopy (\filename -> do
            let inputFile = combine inputDir filename
                outputFile = combine outputDir filename
            copyFile inputFile outputFile
            getFileSize inputFile )
        putStrLn $ "Copied " ++ show (sum sizes) ++ " bytes."
    else
        putStrLn "Both directories are the same"

-- | Takes a path to file and returns the file's size in bytes
getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

{- | Handles errors in such cases:

        * User didn't pass at least 2 command line arguments
        * The specified paths don't point to existing directories
-}
handleError :: IOError -> IO ()
handleError e
    | isUserError e = do -- triggered if user didn't at least 2 arguments
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " " ++ "<inputDirPath> <outputDirPath>"
    | isDoesNotExistError e = case ioeGetFileName e of
        Just path -> putStrLn $ "Directory does not exist at: " ++ path
        Nothing -> putStrLn "File or directory does not exist"
    | otherwise = ioError e
