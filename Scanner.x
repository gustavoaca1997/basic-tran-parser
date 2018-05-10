{
module Main (main) where
}

%wrapper "posn"

tokens :-

  $white+       ;
  True          {\ap s -> TkTrue ap}
  False         {\ap s -> TkFalse ap}

{
data Token =
    TkTrue AlexPosn
    | TkFalse AlexPosn
    deriving (Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}