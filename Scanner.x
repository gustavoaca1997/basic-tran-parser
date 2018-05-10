{
module Main (main) where
}

%wrapper "posn"

tokens :-
  $white+       ;
  True          {\ap s -> TkTrue ap}
  False         {\ap s -> TkFalse ap}
  .             {\ap s -> TkErr ap}
{
data Token =
    TkTrue AlexPosn
    | TkFalse AlexPosn
    | TkErr AlexPosn
    
instance Show Token where
  show (TkTrue (AlexPn _ l c)) = "TkTrue " ++ show l ++ " " ++ show c
  show (TkFalse (AlexPn _ l c)) = "TkFalse " ++ show l ++ " " ++ show c  
  show (TkErr (AlexPn _ l c)) = "Error en " ++ show l ++ ":" ++ show c

main = do
  s <- getContents
  print (alexScanTokens s)
}