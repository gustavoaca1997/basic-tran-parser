{
module Main (main) where
}

%wrapper "posn"

$letras = [a-zA-Z]
$numeros = 0-9
$alphanum = [a-zA-Z0-9]

tokens :-
  -- spaces
  $white+       ;
  -- Palabras reservadas
  with              {\ap s -> TkObject TkWith ap}
  end               {\ap s -> TkObject TkEnd ap}
  var               {\ap s -> TkObject TkVar ap}
  while             {\ap s -> TkObject TkWhile ap}
  for               {\ap s -> TkObject TkFor ap}
  begin             {\ap s -> TkObject TkBegin ap}
  read              {\ap s -> TkObject TkRead ap}
  print             {\ap s -> TkObject TkPrint ap}

  -- Tipos
  bool              {\ap s -> TkObject TkBool ap}
  int               {\ap s -> TkObject TkInt ap}
  char              {\ap s -> TkObject TkChar ap}
  array             {\ap s -> TkObject TkArray ap}

  -- numbers
  [0-9]+[a-zA-Z]+   {\ap s -> TkObject (TkErr s) ap}
  [0-9]+            {\ap s -> TkObject (TkNum s) ap}

  -- caracteres
  '.'               {\ap s -> TkObject (TkCaracter s) ap}
  '\\[\\nt\']'      {\ap s -> TkObject (TkCaracter s) ap}

  -- 
  -- Booleans
  true              {\ap s -> TkObject TkTrue ap}
  false             {\ap s -> TkObject TkFalse ap}

  -- separadores
  \,                 {\ap s -> TkObject TkComa ap}
  \.                {\ap s -> TkObject TkPunto ap}
  \;                {\ap s -> TkObject TkPuntoYComa ap}
  ::                {\ap s -> TkObject TkConcatenacion ap}
  :                 {\ap s -> TkObject TkDosPuntos ap}
  \(                {\ap s -> TkObject TkParAbre ap}
  \)                {\ap s -> TkObject TkParCierra ap}
  \[                {\ap s -> TkObject TkCorcheteAbre ap}
  \]                {\ap s -> TkObject TkCorcheteCierra ap}
  \{                {\ap s -> TkObject TkLlaveAbre ap}
  \}                {\ap s -> TkObject TkLlaveCierra ap}
  \-\>               {\ap s -> TkObject TkHacer ap}
  \<\-               {\ap s -> TkObject TkAsignacion ap}

  -- Operadores
  \+\+              {\ap s -> TkObject TkSiguienteCar ap}
  \+                {\ap s -> TkObject TkSuma ap}
  \-\-              {\ap s -> TkObject TkAnteriorCar ap}
  \-                {\ap s -> TkObject TkResta ap}
  \*                {\ap s -> TkObject TkMult ap}
  \/                 {\ap s -> TkObject TkDiv ap}
  \%                 {\ap s -> TkObject TkMod ap}
  \/\\               {\ap s -> TkObject TkConjuncion ap}
  \\\/               {\ap s -> TkObject TkDisyuncion ap}
  not               {\ap s -> TkObject TkNegacion ap}
  \/=                {\ap s -> TkObject TkDesigual ap}
  \<                {\ap s -> TkObject TkMenor ap}
  \<=                {\ap s -> TkObject TkMenorIgual ap}
  \>                {\ap s -> TkObject TkMayor ap}
  \>=                {\ap s -> TkObject TkMayorIgual ap}
  =                 {\ap s -> TkObject TkIgual ap}
  \#                 {\ap s -> TkObject TkValorAscii ap}
  \$                {\ap s -> TkObject TkShift ap}

  -- id
  $letras($alphanum*)   {\ap s -> TkObject (TkId s) ap}

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
  -- Palabras Reservadas
    TkWith
    | TkEnd
    | TkVar
    | TkWhile
    | TkFor
    | TkIf
    | TkOtherwise
    | TkOf
    | TkBegin
    | TkPrint
    | TkRead

    -- Tipos
    | TkInt
    | TkBool
    | TkChar
    | TkArray

    -- Literales
    | TkCaracter String
    | TkTrue
    | TkId String
    | TkFalse
    | TkNum String

    -- Separadores
    | TkComa
    | TkPunto
    | TkPuntoYComa
    | TkDosPuntos
    | TkParAbre
    | TkParCierra
    | TkCorcheteAbre
    | TkCorcheteCierra
    | TkLlaveAbre
    | TkLlaveCierra
    | TkHacer
    | TkAsignacion

    -- Operadores
    | TkSuma
    | TkResta
    | TkMult
    | TkDiv
    | TkMod
    | TkConjuncion
    | TkDisyuncion
    | TkNegacion
    | TkDesigual
    | TkMenor
    | TkMenorIgual
    | TkMayor
    | TkMayorIgual
    | TkIgual
    | TkSiguienteCar
    | TkAnteriorCar
    | TkValorAscii
    | TkConcatenacion
    | TkShift

    -- Error
    | TkErr String
    deriving (Eq, Show)


-- par (Token, AlexPosn)
data TkObject = TkObject Token AlexPosn deriving (Eq)
instance Show TkObject where
  -- Ejm: TkNum(30) 3 2
  show (TkObject (TkNum num) (AlexPn _ l c)) = "TkNum(" ++ num ++ ") " ++ show l ++ " " ++ show c
  -- Ejm: TkId('beta') 3 2
  show (TkObject (TkId i) (AlexPn _ l c)) = "TkId(" ++ show i ++ ") " ++ show l ++ " " ++ show c
  -- Ejm: Error: Caracter inesperado '?' en la fila 3 2
  show (TkObject (TkErr tk) (AlexPn _ l c)) = "Error: Caracter inesperado " ++ show (tk!!0) ++ " en la fila " ++ show l ++ ", columna " ++ show c
  -- Ejm: TkCaracter('p') 2 3
  show (TkObject (TkCaracter s) (AlexPn _ l c)) = "TkCaracter(" ++ s ++ ") " ++ show l ++ " " ++ show c
  -- Ejm: TkWhile 3 2
  show (TkObject tk (AlexPn _ l c)) = show tk ++ " " ++ show l ++ " " ++ show c

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- Acciones IO

-- Imprimir los tokens en el formato especificado
-- print_action s =
--   putStr (intercalate "" 
--     (init [ intercalate "" (columnas linea) ++ "\n" | linea <- [1..length tokens]])
--   )
--       where
--         tokens = alexScanTokens s                                                   -- [TkObject]
--         columnas linea = map (\tk -> show tk ++ final tk linea) $ cols linea        -- [show TkObject]
--         final tk linea = if tk /= last (cols linea) then ", " else ""               -- ", " o ""
--         cols linea' = filter (\(TkObject _ (AlexPn _ l _)) -> l == linea') tokens   -- columnas de la fila

group :: [TkObject] -> [[TkObject]]
group [] = []
--group [a] = [[a]]
group ent@((TkObject _ (AlexPn _ a _)):_) = equalA:(group rest)
  where equalA = takeWhile (\(TkObject _ (AlexPn _ a' _)) -> a' == a) ent
        rest = drop (length equalA) ent

formatln' :: [TkObject] -> String
formatln' xs = concatMap (\tk -> show tk ++ final tk) xs
        where final tk = if tk /= last xs then ", " else "\n"

formatln :: [[TkObject]] -> String
formatln = concatMap (formatln')

print_action = putStr . formatln . group . alexScanTokens

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

main = do
  s <- getContents;
  print_action s;
}