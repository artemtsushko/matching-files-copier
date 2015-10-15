import System.Directory
import System.Environment
import System.FilePath.Posix (combine)
import System.Posix
import Control.Monad
import Data.List

main = do
    [inputDirectory, outputDirectory] <- getArgs
    directoryContents <- getDirectoryContents inputDirectory
    putStr "enter file beginning: "
    beginning <- getLine
    let contentsWithBeginning = filterBeginsWith beginning directoryContents
        matchingPaths = map (combine inputDirectory) contentsWithBeginning
    filesToCopy <- filterIsFile matchingPaths
    sizes <- forM filesToCopy (\filename -> do
        size <- getFileSize filename
        putStrLn $ filename ++ " " ++ show size
        return size)
    print $ sum sizes


filterBeginsWith :: String -> [FilePath] -> [FilePath]
filterBeginsWith beginning = filter (beginning `isPrefixOf`)


filterIsFile :: [FilePath] -> IO [FilePath]
filterIsFile files = do
    filenames <- forM files (\filename -> do
        isFile <- doesFileExist filename
        return $ if isFile then filename else "")
    return $ filter (not . null) filenames


getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)
