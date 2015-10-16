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
import System.Posix
import Control.Monad
import Data.List

main = do
    [inputDir, outputDir] <- getArgs
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
-- debug
    print $ zip filesToCopy sizes

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

