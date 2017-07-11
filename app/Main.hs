{- |
 -  Main module
 -}
module Main(
    -- * @Main@
    main
)
where

import FileCompressor
import System (getArgs)

{- |
   Given the following parameters:
   - File to read from
   - File to write to
   - Transformation to apply (compress or decompress)

   It applies the specified transformation to the input data and 
   writes it to the expected output location
 -}
main :: IO ()
main = do
    args <- getArgs
    let source = args !! 0
        target = args !! 1
        mode   = if (args !! 2) == "compress" then True else False
    dataRead <- readFromFile source
    printFile target $ transform mode dataRead
