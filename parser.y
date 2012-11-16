{
module Parser where
import Scanner
import Datatypes
}

%name testpar
%tokentype { Token }
%error { parseError }


%token
	"&" {AND}
	"|" {OR}
	":=" {ASSIGN}
	";" {SEMI}
	">" {GTHAN}
	">=" {GEQUAL}
	"<" {LTHAN}
  	"<=" {LEQUAL}
	"=" {EQUAL} 
	"!=" {NEQUAL}
	"+" {PLUS}
	"-" {MINUS}
	"*" {MUL}
	"/" {DIV}
	"!" {NOT}
	"(" {LPAREN}
	")" {RPAREN}
	"{" {LBRACE}
	"}" {RBRACE}
	"[" {LBRACKET}
	"]" {RBRACKET}
	":" {COLON}
	"if" {IF}
	"then" {THEN}
	"else" {ELSE}
	"fi" {FI}
	"while" {WHILE}
	"do" {DO}
	"od" {OD}
	"skip" {SKIP}
	"write" {WRITE}
	"read" {READ}
	"program" {PROGRAM}
	"end" {END}
  	"true" {TRUE}
	"false" {FALSE}
	"int" {INT}
	integer_literal {INTLITERAL $$}
	identifier {IDENTIFIER $$}
%%

Program		: "program" DeclBody StmtList "end"	{Program $2 $3}

DeclBody	: 		{EmptyDeclBody}
		| DeclList	{DeclBody $1}

DeclList 	: Decl		{DeclList $1 NoDecl}
		| Decl DeclList {DeclList $1 $2} 

Decl 		: "int" identifier ";"	{Decl $2}
		| "int" identifier "[" integer_literal "]" ";" {DeclArray $2 $4}

StmtList	: Stmt		{StmtList $1 NoStmt}
		| Stmt StmtList {StmtList $1 $2}		

Stmt		: identifier ":=" Aexpr ";"				{StmtAssign $1 $3}
		| identifier "[" Aexpr "]" ":=" Aexpr ";"		{StmtAssignArray $1 $3 $6}
		| "skip" ";"						{StmtSkip}
		| "read" identifier ";"					{StmtRead $2}
		| "read" identifier "[" Aexpr "]" ";"			{StmtReadArray $2 $4}
		| "write" Aexpr ";"					{StmtWrite $2}
		| "if" Bexpr "then" StmtList "else" StmtList "fi" 	{StmtIf $2 $4 $6}
		| "while" Bexpr "do" StmtList "od"			{StmtWhile $2 $4}

Aexpr	: Aexpr1		{Aexpr1 $1} 
	| Aexpr "+" Aexpr1 	{Plus $1 $3}
	| Aexpr "-" Aexpr1	{Minus $1 $3}

Aexpr1	: Aexpr2		{Aexpr2 $1}
	| Aexpr1 "*" Aexpr2	{Mul $1 $3}
	| Aexpr1 "/" Aexpr2	{Div $1 $3}

Aexpr2	: "-" Aexpr3		{Neg $2}
	| Aexpr3		{Aexpr3 $1}

Aexpr3 	: identifier			{Identifier $1}
	| identifier "[" Aexpr "]"	{IdentifierArray $1 $3}
	| integer_literal		{IntegerLiteral $1}
	| "(" Aexpr ")"			{ABrack $2}
	
Bexpr	: Bexpr1		{Bexpr1 $1}
	| Bexpr "|" Bexpr1	{Or $1 $3}

Bexpr1  : Bexpr2		{Bexpr2 $1}
	| Bexpr1 "&" Bexpr2	{And $1 $3}

Bexpr2	: Aexpr ">" Aexpr	{GreatThan $1 $3}
	| Aexpr "<" Aexpr	{LessThan $1 $3}
	| Aexpr ">=" Aexpr	{GreatEqual $1 $3}
	| Aexpr "<=" Aexpr 	{LessEqual $1 $3}
	| Aexpr "=" Aexpr 	{Equal $1 $3}
	| Aexpr "!=" Aexpr 	{NotEqual $1 $3}
	| "!" Bexpr		{Not $2}
	| "true"		{Boolean True}
	| "false"		{Boolean False}
	| "(" Bexpr ")"		{BBrack $2}
{
parseError :: [Token] -> a
parseError _ = error "Parse error"




getTree :: String -> Program
getTree s = testpar (alexScanTokens s)

getStmtList :: Program -> StmtList
getStmtList (Program _ stmt) = stmt

getDeclList :: Program -> DeclList
getDeclList (Program (DeclBody decl) _) = decl
getDeclList (Program (EmptyDeclBody) _) = NoDecl 

getStmt (StmtList stmt stmtlist) = (stmt, stmtlist)

try :: String -> IO ()
try s = do
	let parseTree = testpar (alexScanTokens s)
	putStrLn ("parseTree: " ++ show(parseTree))
	print "done"

}
		


	


 



