module Main where
import Lex
import Parser
import System.Environment

main = do
    args <- getArgs
    filecontents <- readFile $ head args
    let tokens = scanTokens filecontents
    print tokens
    print $ parser tokens