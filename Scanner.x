{
module Main (main) where
}

%wrapper "posn"

tokens :-
  -- spaces
  $white+       ;

  -- numbers
  [0-9]+[a-zA-Z]+   {\ap s -> TkObject (TkErr s) ap}
  [0-9]+            {\ap s -> TkObject (TkNum s) ap}

  -- Booleans
  True              {\ap s -> TkObject TkTrue ap}
  False             {\ap s -> TkObject TkFalse ap}
  .                 {\ap s -> TkObject (TkErr s) ap}
{
data Token =
    TkTrue
    | TkFalse
    | TkErr String
    | TkNum String
    deriving (Show)

data TkObject = TkObject Token AlexPosn
instance Show TkObject where
  show (TkObject (TkNum num) (AlexPn _ l c)) = "TkNum(" ++ num ++ ") " ++ show l ++ " " ++ show c
  show (TkObject (TkErr tk) (AlexPn _ l c)) = "Error: Caracter inesperado " ++ show (tk!!0) ++ " en la fila " ++ show l ++ ", columna " ++ show c
  show (TkObject tk (AlexPn _ l c)) = show tk ++ " " ++ show l ++ " " ++ show c
    
-- instance Show Token where
--   show (TkNum (AlexPn _ l c) num) = "TkNum(" ++ num ++ ") " ++ show l ++ " " ++ show c
--   show (TkTrue (AlexPn _ l c)) = "TkTrue " ++ show l ++ " " ++ show c
--   show (TkFalse (AlexPn _ l c)) = "TkFalse " ++ show l ++ " " ++ show c  
--   show (TkErr (AlexPn _ l c)) = "(Lexer) error on " ++ show l ++ ":" ++ show c

main = do
  s <- getContents
  print (alexScanTokens s)
}