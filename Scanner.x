{
module Main (main) where
}

%wrapper "basic"

tokens :-

  $white+				;
  True                  {\s -> TkTrue}
  False                 {\s -> TkFalse}

{
data Token =
    TkTrue
    | TkFalse
    deriving (Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}