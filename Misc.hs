module Misc where

import qualified Data.ByteString as B
import System.Directory (doesFileExist)

readFile :: FilePath -> IO (Either String B.ByteString)
readFile path = do fileExist <- doesFileExist path 
                   if fileExist then do src <- B.readFile path
                                        return (Right src)
                                else return (Left $ "File '" ++ path ++ "' doesn't exist.")
