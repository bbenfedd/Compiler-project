{

  open TokenJava
(*  open String *)
(*  open Str *)
  exception LexicalError

}

(* Macro-definitions *)
let minuscule = ['a'-'z']
let majuscule = ['A'-'Z']
let chiffre = ['0'-'9']
let underscore = '_'
let underscores = underscore (underscore)*
let integerTypeSuffix = 'l' | 'L'

(* Nombres entiers *)

(* decimalIntegerLiteral *)
let nonZeroDigit = ['1'-'9']
let digit = '0' | nonZeroDigit
let digitOrUnderscore = digit | underscore
let digitsAndUnderscores = digitOrUnderscore (digitOrUnderscore)*
let digits = digit | digit (digitsAndUnderscores)? digit
let decimalNumeral = '0' | nonZeroDigit (digits)? | nonZeroDigit underscores digits
                             
                             
let decimalIntegerLiteral = decimalNumeral (integerTypeSuffix)?

(* HexIntegerLiteral *) 

let hexDigit = chiffre | ['a'-'f'] | ['A'-'F']
let hexDigitOrUnderscore = hexDigit | underscore
let hexDigitsAndUnderscores = hexDigitOrUnderscore (hexDigitOrUnderscore)*
let hexDigits = hexDigit | hexDigit (hexDigitsAndUnderscores)? hexDigit
let hexNumeral = '0' ('x' | 'X') hexDigits 

let hexIntegerLiteral = hexNumeral (integerTypeSuffix)?


(* OctalIntegerLiteral *)


let octalDigit = ['0'-'7']
let octalDigitOrUnderScore = octalDigit | underscore
let octalDigitsAndUndescores = octalDigitOrUnderScore (octalDigitOrUnderScore)*
let octalDigits = octalDigit | octalDigit (octalDigitsAndUndescores)? octalDigit
let octalNumeral = '0' octalDigits | '0' underscores octalDigits

let octalIntegerLiteral = octalNumeral (integerTypeSuffix)?


(* binaryIntegerLiteral *)
let binaryDigit = '0' | '1'
let binaryDigitOrUndescore = binaryDigit | underscore
let binaryDigitsAndUnderscores = binaryDigitOrUndescore (binaryDigitOrUndescore)*
let binaryDigits = binaryDigit | binaryDigit (binaryDigitsAndUnderscores)? binaryDigit
let binaryNumeral = '0' ('b' | 'B') binaryDigits

let binaryIntegerLiteral = binaryNumeral (integerTypeSuffix)?

(* IntegerLiteral *)
let integerLiteral = decimalNumeral | hexIntegerLiteral | octalIntegerLiteral | binaryIntegerLiteral



(* DecimalFloatingPointLiteral*)
let sign = '+' | '-'
let exponentIndicator = 'e' | 'E'
let floatTypeSuffix = 'f' | 'F' | 'd' | 'D'
let signedInteger = (sign)? digits
let exponentPart = exponentIndicator signedInteger

let decimalFloatingPointLiteral = digits '.' (digits)? (exponentPart)? (floatTypeSuffix)?
        | '.' digits (exponentPart)? (floatTypeSuffix)?
        | digits exponentPart (floatTypeSuffix)?
        | digits (exponentPart)? floatTypeSuffix
        
(* HexadecimalFloatingPointLiteral*)
let binaryExponentIndicator = 'P' | 'p'
let binaryExponent = binaryExponentIndicator signedInteger
let hexSignificand = hexNumeral ('.')? | '0' ('x'|'X') (hexDigits)? '.' hexDigits


let hexadecimalFloatingPointLiteral = hexSignificand binaryExponent (floatTypeSuffix)?

(* FloatingPointLiteral *)
let floatingPointLiteral = decimalFloatingPointLiteral | hexadecimalFloatingPointLiteral

let alphabet = minuscule | majuscule
let alphanum = alphabet | chiffre | '_'
let commentaireBloc = (* A COMPLETER *) "/*" _* "*/" 
let commentaireLigne = "//" [^'\n']* '\n'

(* Analyseur lexical : expression reguliere { action CaML } *)
rule lexer = parse
(* Espace, tabulation, passage a ligne, etc : consommes par l'analyse lexicale *)
  | ['\n' '\t' ' ']+    { lexer lexbuf }
(* Commentaires consommes par l'analyse lexicale *)
  | commentaireBloc  	{ lexer lexbuf }
  | commentaireLigne	{ lexer lexbuf }
(* Structures de blocs *)
  | "("                 { PAROUV }
  | ")"                 { PARFER }
  | "["                 { CROOUV }
  | "]"                 { CROFER }
  | "{"                 { ACCOUV }
  | "}"                 { ACCFER }
(* Separateurs *)
  | ","                 { VIRG }
  | ";"                 { PTVIRG }
(* Operateurs booleens *)
  | "||"                { OPOU }
  | "&&"                { OPET }
  | "!"                 { OPNON }
(* Operateurs comparaisons *)
  | "=="                { OPEG }
  | "!="                { OPNONEG }
  | "<="                { OPSUPEG }
  | "<"                 { OPSUP }
  | ">="                { OPINFEG }
  | ">"                 { OPINF }
(* Operateurs arithmetiques *)
  | "+"                 { OPPLUS }
  | "-"                 { OPMOINS }
  | "*"                 { OPMULT }
  | "/"                 { OPDIV }
  | "%"                 { OPMOD }
  | "."                 { OPPT }
  | "="                 { ASSIGN }
  | "new"               { NOUVEAU }
(* Mots cles : types *)
  | "bool"              { BOOL }
  | "char"              { CHAR }
  | "float"             { FLOAT }
  | "int"               { INT }
  | "String"            { STRING }
  | "void"              { VOID }
(* Mots cles : instructions *)
  | "while"		{ TANTQUE }
  | "if"		{ SI }
  | "else"		{ SINON }
  | "return"		{ RETOUR }
(* Mots cles : constantes *)
  | "true"		{ (BOOLEEN true) }
  | "false"		{ (BOOLEEN false) }
  | "null"		{ VIDE }
(* Nombres entiers : A COMPLETER *)
  | integerLiteral as texte   { (ENTIER (int_of_string texte)) }
 
(* Nombres flottants : A COMPLETER *)
  | floatingPointLiteral as texte     { (FLOTTANT (float_of_string texte)) }
(* Caracteres : A COMPLETER *)
  | "'" _ "'" as texte                   { CARACTERE texte.[1] }
(* Chaines de caracteres : A COMPLETER *)
  | '"' _* '"' as texte                  { CHAINE texte }
(* Identificateurs *)
  | majuscule alphanum* as texte              { TYPEIDENT texte }
  | minuscule alphanum* as texte              { IDENT texte }
  | eof                                       { FIN }
  | _                                         { raise LexicalError }

{

}
