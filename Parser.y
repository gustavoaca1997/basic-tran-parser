{
module Main where
import Lex
}

%name parser
%tokentype { TkObject }
%error { parseError }

%token

-- Reservadas
with        { TkObject TkWith _ }
end         { TkObject TkEnd _  }
var         { TkObject TkVar _  }
while       { TkObject TkWhile _ }
for         { TkObject TkFor _ }
from        { TkObject TkFrom _ }
to          { TkObject TkTo _ }
step        { TkObject TkStep _ }
if          { TkObject TkIf _ }
otherwise   { TkObject TkOtherwise _ }
of          { TkObject TkOf _ }
begin       { TkObject TkBegin _ }
print       { TkObject TkPrint _ }
read        { TkObject TkRead _ }

-- Tipos
int         { TkObject TkInt _ }
bool        { TkObject TkBool _ }
char        { TkObject TkChar _ }
array       { TkObject TkArray _ }

-- Literales
caracter    { TkObject (TkCaracter _) _ }
true        { TkObject TkTrue _ }
id          { TkObject (TkId _) _ }
false       { TkObject TkFalse _ }
num         { TkObject (TkNum _) _ }

-- Separadores
','         { TkObject TkComa _ }
'.'         { TkObject TkPunto _ }
';'         { TkObject TkPuntoYComa _ }
':'         { TkObject TkDosPuntos _ }
'('         { TkObject TkParAbre _ }
')'         { TkObject TkParCierra _ }
'['         { TkObject TkCorcheteAbre _ }
']'         { TkObject TkCorcheteCierra _ }
'->'        { TkObject TkHacer _ }
'<-'        { TkObject TkAsignacion _ }

-- Operadores
'+'         { TkObject TkSuma _ }
'-'         { TkObject TkResta _ }
'*'         { TkObject TkMult _ }
'/'         { TkObject TkDiv _ }
'%'         { TkObject TkMod _ }
and         { TkObject TkConjuncion _ }
or          { TkObject TkDisyuncion _ }
not         { TkObject TkNegacion _ }
'/='        { TkObject TkDesigual _ }
'<'         { TkObject TkMenor _ }
'<='        { TkObject TkMenorIgual _ }
'>'         { TkObject TkMayor _ }
'>='        { TkObject TkMayorIgual _ }
'='         { TkObject TkIgual _ }
'++'        { TkObject TkSiguienteCar _ }
"--"        { TkObject TkAnteriorCar _ }
'#'         { TkObject TkValorAscii _ }
'::'        { TkObject TkConcatenacion _ }
'$'         { TkObject TkShift _ }

-- Grammar
%%
-- Variable Inicial
Exp: With Variables                                { Exp $1 $2 }

With : with                                        { $1 }

-- Declaracion de las variables
Variables : Var Identificadores ':' Tipo               { [Variables (reverse $2) $4] }
    | Var Identificadores ':' Tipo Variables           { (Variables (reverse $2) $4) : $5 }

Var : var   { $1 }

-- Tipo de dato
Tipo : int  { TipoPrimitivo $1 }
    | char  { TipoPrimitivo $1 }
    | bool  { TipoPrimitivo $1 }
    | array '[' ExpArit ']' of Tipo { TipoArreglo $1 $3 $6  }

-- Expresion Arimetica
ExpArit : id    { $1 }
        | num   { $1 }

-- Lista de las variables a declarar e inicializar
Identificadores : Identificadores ',' Inicializacion        { $3:$1 }
                | Inicializacion                            { [$1] }

-- (identificador, literal o identificador)
Inicializacion : id                                       { Declaracion $1 }
                | id '<-' Literal                         { Inicializacion $1 $3 }
                | id '<-' id                              { Inicializacion $1 $3 }

-- Literales
Literal : caracter { $1 }
        | true  { $1 } 
        | false { $1 }
        | num { $1 }
{

parseError :: [TkObject] -> a
parseError _ = error "Parse error"

-- Tipos de datos a retornar
-- Variable inicial
data Exp
    = Exp TkObject [Variables]
    deriving Show

-- Para la declariacion o inicializacion de una variable
data Inicializacion
    = Inicializacion TkObject TkObject -- id <- n
    | Declaracion TkObject              -- id
    deriving Show

-- Tipos de datos
data Tipo =
    TipoPrimitivo TkObject
    | TipoArreglo TkObject TkObject Tipo
    deriving Show

-- Variables
data Variables =
    Variables [Inicializacion] Tipo
    deriving Show

main = getContents >>= print . parser . scanTokens

}