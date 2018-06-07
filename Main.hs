module Main where
import Lex
import Parser

main = getContents >>= print . parser . scanTokens
