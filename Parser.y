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

-- Precedencias
%left '+' '-'
%left '*' '/' '%'
%left NEG
%nonassoc '>' '<' '=' '>=' '<=' '/='
%left and or
%left not
-- Grammar
%%


-- Variable Inicial
Exp: ExpBool                                { $1 }

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


-------------------------------- EXPRESIONES ----------------------------------

-- Expresion Arimetica
ExpArit : ExpArit '+' ExpArit     { Suma $1 $2 $3 }
        | ExpArit '-' ExpArit     { Resta $1 $2 $3 }
        | ExpArit '*' ExpArit     { Mult $1 $2 $3 }
        | ExpArit '/' ExpArit     { Div $1 $2 $3 }
        | ExpArit '%' ExpArit     { Mod $1 $2 $3 }
        | Menos ExpArit %prec NEG { MenosUnario $1 $2 }
        | ParenAbre ExpArit ')'   { $2 }
        | Id                      { IdArit $1 }
        | Num                     { LitArit $1 }

ExpRel : ExpArit '<'  ExpArit     { MenorQue $1 $2 $3 }
       | ExpArit '>'  ExpArit     { MayorQue $1 $2 $3 }
       | ExpArit '<=' ExpArit     { MenorIgualQue $1 $2 $3 }
       | ExpArit '>=' ExpArit     { MayorIgualQue $1 $2 $3 }
       | ExpArit '='  ExpArit     { Igual $1 $2 $3 }
       | ExpArit '/=' ExpArit     { Distinto $1 $2 $3 }

-- Expresiones Booleanas
ExpBool : ExpRel                            { Relacion $1 }   
        | ExpBool OperadorLogico ExpBool    { OperacionLogica $1 $2 $3 }
        | id                                { IdBool $1 }
        | '(' ExpBool ')'                   { $2 }

OperadorLogico :
    and { $1 }
    | or { $1 }
    | not { $1 }

Menos : '-'                       { $1 }
Id    : id                        { $1 }
Num   : num                       { $1 }
ParenAbre : '('                   { $1 }
-- Lista de las variables a declarar e inicializar
Identificadores : Identificadores ',' Inicializacion        { $3:$1 }
                | Inicializacion                            { [$1] }

-- (identificador, literal o identificador)
Inicializacion : id                                       { Declaracion $1 }
                | id '<-' Literal                         { Asignacion $1 (LitArit $3) }
                | id '<-' ExpArit                          { Asignacion $1 $3 }

--------------------------------- INSTRUCCIONES -------------------------------
Instruccion : {- lambda -}                                { EmptyInstr }
            | Condicional                                 { IfInstr $1 }

-- Condicionales
Condicional : If ExpBool '->' Instruccion end                       { If $2 $4 }
            | If ExpBool '->' Instruccion otherwise Instruccion end { IfOtherwise $2 $4 $6 }

If : if         { $1 }
-- Literales
Literal : caracter { $1 }
        | num { $1 }
        | Boolean { $1 }

Boolean : true      { $1 }
        | false     { $1 }
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
    = Asignacion TkObject ExpArit -- id <- n, id <- 2 + x
    | Declaracion TkObject              -- id
    deriving Show

-- Tipos de datos
data Tipo =
    TipoPrimitivo TkObject
    | TipoArreglo TkObject ExpArit Tipo
    deriving Show

-- Variables
data Variables =
    Variables [Inicializacion] Tipo
    deriving Show

data ExpArit =
    Suma ExpArit TkObject ExpArit
    | Resta ExpArit TkObject ExpArit
    | Mult ExpArit TkObject ExpArit
    | Div ExpArit TkObject ExpArit
    | Mod ExpArit TkObject ExpArit
    | MenosUnario TkObject ExpArit
    | LitArit TkObject
    | IdArit  TkObject
    deriving Show

data ExpRel =
    MenorQue ExpArit TkObject ExpArit
    | MayorQue ExpArit TkObject ExpArit
    | MenorIgualQue ExpArit TkObject ExpArit
    | MayorIgualQue ExpArit TkObject ExpArit
    | Igual ExpArit TkObject ExpArit
    | Distinto ExpArit TkObject ExpArit
    deriving Show

data ExpBool =
    Relacion ExpRel -- 2 + n <= x
    | OperacionLogica ExpBool TkObject ExpBool  -- B and (x > 2)
    | IdBool TkObject   -- if es_string
    deriving Show

data Instruccion =
    IfInstr IfInstr
    | EmptyInstr
    deriving Show

data IfInstr =
    If ExpBool Instruccion
    | IfOtherwise ExpBool Instruccion Instruccion
    deriving Show

main = getContents >>= print . parser . scanTokens

}
