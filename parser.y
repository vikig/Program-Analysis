{
module Parser where
import Scanner
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

DeclList 	: Decl		{SingleDecl $1}
		| Decl DeclList {DeclList $1 $2} 

Decl 		: "int" identifier ";"	{Decl $2}
		| "int" identifier "[" Aexpr "]" ";" {DeclArray $2 $4}

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

data Program
	= Program DeclBody StmtList
	deriving(Show, Eq)

data DeclBody
	= DeclBody DeclList
	| EmptyDeclBody
	deriving(Show, Eq)

data DeclList
	= DeclList Decl DeclList
	| SingleDecl Decl
	| NoDecl
	deriving(Show, Eq)

data Decl
	= Decl Identifier
	| DeclArray Identifier Aexpr
	deriving(Show, Eq)

data StmtList
	= StmtList Stmt StmtList
	| NoStmt
	deriving(Show, Eq)

data Stmt
	= StmtAssign Identifier Aexpr
	| StmtAssignArray Identifier Aexpr Aexpr
	| StmtSkip
	| StmtIf Bexpr StmtList StmtList
	| StmtRead Identifier
	| StmtReadArray Identifier Aexpr
	| StmtWrite Aexpr
	| StmtWhile Bexpr StmtList
	deriving(Show, Eq)

data Aexpr
	= Aexpr1 Aexpr1
	| Plus Aexpr Aexpr1
	| Minus Aexpr Aexpr1
	deriving(Show, Eq)

data Aexpr1
	= Aexpr2 Aexpr2
	| Mul Aexpr1 Aexpr2
	| Div Aexpr1 Aexpr2
	deriving(Show, Eq)

data Aexpr2
	= Neg Aexpr3
	| Aexpr3 Aexpr3
	deriving(Show, Eq)

data Aexpr3
	= Identifier Identifier
	| IdentifierArray Identifier Aexpr
	| IntegerLiteral IntegerLiteral
	| ABrack Aexpr
	deriving(Show, Eq)

data Bexpr 
	= Bexpr1 Bexpr1
	| Or Bexpr Bexpr1
	deriving(Show, Eq)

data Bexpr1
	= Bexpr2 Bexpr2
	| And Bexpr1 Bexpr2
	deriving(Show, Eq)

data Bexpr2
	= GreatThan Aexpr Aexpr
	| LessThan Aexpr Aexpr
	| GreatEqual Aexpr Aexpr
	| LessEqual Aexpr Aexpr
	| Equal Aexpr Aexpr
	| NotEqual Aexpr Aexpr
	| Not Bexpr
	| Boolean Boolean
	| BBrack Bexpr
	deriving(Show, Eq)

type Identifier	= String
type IntegerLiteral = Int
type Boolean = Bool


getTree :: String -> Program
getTree s = testpar (alexScanTokens s)

getStmtList :: Program -> StmtList
getStmtList (Program decl stmt) = stmt


getStmt (StmtList stmt stmtlist) = (stmt, stmtlist)

try :: String -> IO ()
try s = do
	let parseTree = testpar (alexScanTokens s)
	putStrLn ("parseTree: " ++ show(parseTree))
	print "done"

}
		


	


 



