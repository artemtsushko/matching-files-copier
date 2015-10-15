import System.Directory
import System.Environment
import System.FilePath.Posix (combine)
import System.Posix
import Control.Monad
import Data.List

main = do
    [inputDirectory, outputDirectory] <- getArgs
    putStr "enter file beginning: "
    beginning <- getLine
    directoryContents <- getDirectoryContents inputDirectory
    filesToCopy <- filterM (doesFileExist)
                 . map (combine inputDirectory)
                 . filter (beginning `isPrefixOf`)
                 $ directoryContents
    sizes <- forM filesToCopy getFileSize
-- debug
    print $ zip filesToCopy sizes
    print $ sum sizes

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)
