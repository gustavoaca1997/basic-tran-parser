{
module Parser where
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
S: IncAlcance                { Programa $1 }

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

Expresion : ExpArit               { ExpArit $1 }
        | ExpBool                 { ExpBool $1 }
        | ExpChar                 { ExpChar $1 }
        | ExpArray                { ExpArray $1 }

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
        | true                              { LitBool $1 }
        | false                              { LitBool $1 }

OperadorLogico :
    and { $1 }
    | or { $1 }
    | not { $1 }

-- Expresiones con caracteres
ExpChar : ExpChar '++'          { SiguienteChar $1 $2 }
        | ExpChar "--"          { AnteriorChar $1 $2 }
        | '#' ExpChar                   { Ascii $1 $2 }
        | '(' ExpChar ')'               { $2 }
        | id                            { IdChar $1 }
        | caracter                      { LitChar $1 }

-- Expresiones con arreglos
ExpArray : ExpArray '::' ExpArray       { ConcatenacionArray $1 $2 $3 }
        | '$' ExpArray                  { ShiftArray $1 $2 }
        | ExpArray '[' ExpArit ']'      { IndexacionArray $1 $3 }
        | id                            { IdArray $1 }
        | '(' ExpArray ')'              { $2 }


Menos : '-'                       { $1 }
Id    : id                        { $1 }
Num   : num                       { $1 }
ParenAbre : '('                   { $1 }
-- Lista de las variables a declarar e inicializar
Identificadores : Identificadores ',' Inicializacion        { $3:$1 }
                | Inicializacion                            { [$1] }

-- (identificador, literal o identificador)
Inicializacion : id                                       { Declaracion $1 }
                | Asignacion                              { $1 }

--------------------------------- INSTRUCCIONES -------------------------------
Instruccion : {- lambda -}                                { EmptyInstr }
            | Condicional                                 { IfInstr $1 }
            | IterDet                                     { ForInstr $1 }
            | IteracionInd                                { $1 }
            | Asignacion ';'                              { AsignacionInstr $1 }
            | IOInstr ';'                                 { IOInstr $1 }
            | IncAlcance                                  { IncAlcanceInstr $1 }
            | PuntoInstr ';'                              { PuntoInstr $1 }
            | Instruccion Instruccion                     { Secuenciacion $1 $2 }

-- Condicionales
Condicional : If ExpBool '->' Instruccion end                       { If $2 $4 }
            | If ExpBool '->' Instruccion otherwise Instruccion end { IfOtherwise $2 $4 $6 }

-- Iteracion Determinada
IterDet : For Id from ExpArit to ExpArit '->' Instruccion end              { For $1 $2 $4 $6 $8 }
        | For Id from ExpArit to ExpArit step ExpArit '->' Instruccion end { ForStep $1 $2 $4 $6 $8 $10 }

-- Instrucciones I/O
IOInstr : Print Expresion           { Print $1 $2 }
        | Read  Id                  { Read $1 $2 }

-- Asignacion
Asignacion : 
    id '<-' Literal                         { Asignacion $1 (ExpArit (LitArit $3)) }
    | id '<-' ExpArit                          { Asignacion $1 (ExpArit $3) }
    | id '<-' ExpBool                          { Asignacion $1 (ExpBool $3) }

-- Iteración Indeterminada
IteracionInd : While ExpBool '->' Instruccion end            { WhileInstr $2 $4 }

-- Alcance
IncAlcance : With Variables begin Instruccion end            { ConDeclaracion $1 $2 $4 }
           | Begin Instruccion end                           { SinDeclaracion $1 $2 }

-- Instruccion Punto
PuntoInstr : id '.' Num                                      { Punto $1 $2 $3  }
           | id '.' id                                       { Punto $1 $2 $3  }
           | id '.' ExpArit                                  { PuntoExp $1 $2 $3  }

Begin : begin   { $1 }
While : while   { $1 }
Print : print   { $1 }
Read : read   { $1 }
If : if         { $1 }
For : for       { $1 }

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
data Programa
    =  Programa IncAlcanceInstr
    deriving Show

-- Para la declariacion o inicializacion de una variable
data Inicializacion
    = Asignacion TkObject Expresion -- id <- n, id <- 2 + x
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

-------------------------------- EXPRESIONES ----------------------------------

data Expresion =
    ExpArit ExpArit
    | ExpBool ExpBool
    | ExpChar ExpChar
    | ExpArray ExpArray
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
    | LitBool TkObject  -- True, False
    deriving Show

data ExpChar =
    SiguienteChar ExpChar TkObject
    | AnteriorChar ExpChar TkObject
    | Ascii TkObject ExpChar
    | IdChar TkObject
    | LitChar TkObject
    deriving Show

data ExpArray =
    ConcatenacionArray ExpArray TkObject ExpArray
    | ShiftArray TkObject ExpArray
    | IndexacionArray ExpArray ExpArit
    | IdArray TkObject
    deriving Show

--------------------------------- INSTRUCCIONES -------------------------------

data Instruccion =
    IfInstr IfInstr
    | ForInstr ForInstr
    | WhileInstr ExpBool Instruccion
    | IOInstr IOInstr
    | AsignacionInstr Inicializacion
    | IncAlcanceInstr IncAlcanceInstr
    | Secuenciacion Instruccion Instruccion
    | PuntoInstr PuntoInstr
    -- | Asignacion (ver arriba en inicializacion)
    | EmptyInstr
    deriving Show

data IfInstr =
    If ExpBool Instruccion
    | IfOtherwise ExpBool Instruccion Instruccion
    deriving Show

data ForInstr =
    For
        TkObject    -- posicion
        TkObject    -- id
        ExpArit     -- from
        ExpArit     -- to
        Instruccion -- Instruccion
    | ForStep
        TkObject    -- posicion
        TkObject    -- id
        ExpArit     -- from
        ExpArit     -- to
        ExpArit     -- step
        Instruccion -- Instruccion
    deriving Show

data IOInstr =
    Print TkObject Expresion
    | Read TkObject TkObject
    deriving Show

data IncAlcanceInstr =
    ConDeclaracion TkObject [Variables] Instruccion
    | SinDeclaracion TkObject Instruccion
    deriving Show

data PuntoInstr =
    Punto
        TkObject -- Solo id
        TkObject -- . position
        TkObject -- id o num
    | PuntoExp
        TkObject -- Solo id
        TkObject -- . position
        ExpArit  -- Expresion aritmetica
    deriving Show
}
