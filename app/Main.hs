module Main where

import Parser
import Petsc
import System.Environment
usage = "usage: stack exec mat2petsc <filename>"

main :: IO ()
main = do
    putStrLn "Parsing..."
    args <- getArgs
    case length args of
        1 -> parseMain (head args)
        _ -> putStrLn usage

-- parsing has the effect of printing the parse tree as a string
-- and writing the translated file to out.c, and a Makefile
parseMain :: String -> IO ()
parseMain filename = do
    putStrLn ("Reading " ++ filename)
    s <- readFile filename
    -- parseTest parser s -- run_parser is more robust with errors
    let m = run_parser parser s
    putStrLn (show m)
    let fn = "out.c"
    writeFile fn (disfigure m)
    writeFile "Makefile" (build m)
    putStrLn ("Wrote to " ++ fn ++ " and Makefile")
    putStrLn ("Build with 'make out' and run with './out'")

