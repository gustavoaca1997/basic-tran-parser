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
caracter    { TkObject (TkCaracter $$) _ }
true        { TkObject TkTrue _ }
id          { TkObject (TkId _) _ }
false       { TkObject TkFalse _ }
num         { TkObject (TkNum $$) _ }

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
-- var no puede aparecer de primero
%%
Exp: With Variables              { Exp $1 $2 }
With : with                             { $1 }
Variables : var Identificadores          { reverse $2 }
Identificadores : Identificadores ',' id     { $3:$1 }
                | id                     { [$1] }
{

parseError :: [TkObject] -> a
parseError _ = error "Parse error"

data Exp
    = Exp TkObject [TkObject]
    deriving Show

main = getContents >>= print . parser . scanTokens

}