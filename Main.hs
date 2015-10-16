import System.Directory
import System.Environment
import System.FilePath.Posix (combine)
import System.Posix
import Control.Monad
import Data.List

main = do
    [inputDir, outputDir] <- getArgs
    putStr "enter file beginning: "
    beginning <- getLine
    directoryContents <- getDirectoryContents inputDir
    filesToCopy <- filterM (doesFileExist . (combine inputDir))
                 . filter (beginning `isPrefixOf`)
                 $ directoryContents
    forM_ filesToCopy (\filename -> do
        copyFile (combine inputDir filename) (combine outputDir filename))
    sizes <- mapM getFileSize $ map (combine inputDir) filesToCopy
    putStrLn $ "Copied " ++ (show $ sum sizes) ++ " bytes."
-- debug
    print $ zip filesToCopy sizes

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)
