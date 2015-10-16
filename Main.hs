import System.Directory
import System.Environment
import System.FilePath.Posix (combine)
import System.Posix
import Control.Monad
import Data.List

main = do
    [inputDir, outputDir] <- getArgs
    beginning <- putStr "enter file beginning: " >> getLine
    filesToCopy <- filterM (doesFileExist . (combine inputDir))
                 . filter (beginning `isPrefixOf`)
               =<< getDirectoryContents inputDir
    sizes <- forM filesToCopy (\filename -> do
        let inputFile = combine inputDir filename
            outputFile = combine outputDir filename
        copyFile inputFile outputFile
        getFileSize inputFile )
    putStrLn $ "Copied " ++ (show $ sum sizes) ++ " bytes."
-- debug
    print $ zip filesToCopy sizes

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)
