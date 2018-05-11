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

  -- Cualquier cosa
  .                 {\ap s -> TkObject (TkErr s) ap}
{
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
-- Funciones necesarias de Data.List
prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- Codigo

-- tipos de token
data Token =
    TkTrue
    | TkFalse
    | TkErr String
    | TkNum String
    deriving (Eq, Show)


-- par (Token, AlexPosn)
data TkObject = TkObject Token AlexPosn deriving (Eq)
instance Show TkObject where
  -- Ejm: TkNum(30) 3 2
  show (TkObject (TkNum num) (AlexPn _ l c)) = "TkNum(" ++ num ++ ") " ++ show l ++ " " ++ show c
  -- Ejm: Error: Caracter inesperado '?' en la fila 3 2
  show (TkObject (TkErr tk) (AlexPn _ l c)) = "Error: Caracter inesperado " ++ show (tk!!0) ++ " en la fila " ++ show l ++ ", columna " ++ show c
  -- Ejm: TkWhile 3 2
  show (TkObject tk (AlexPn _ l c)) = show tk ++ " " ++ show l ++ " " ++ show c

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- Acciones IO

-- Imprimir los tokens en el formato especificado
print_action s =
  putStr (intercalate "" 
    (init [ intercalate "" (columnas linea) ++ "\n" | linea <- [1..length tokens]])
  )
      where
        tokens = alexScanTokens s                                                   -- [TkObject]
        columnas linea = map (\tk -> show tk ++ final tk linea) $ cols linea        -- [show TkObject]
        final tk linea = if tk /= last (cols linea) then ", " else ""               -- ", " o ""
        cols linea' = filter (\(TkObject _ (AlexPn _ l _)) -> l == linea') tokens   -- columnas de la fila

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

main = do
  s <- getContents;
  print_action s;
}