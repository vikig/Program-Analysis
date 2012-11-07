{
module Main (main) where

}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "&"					{ \s -> AND }
  "|"				    	{ \s -> OR }
  ":="					{ \s -> ASSIGN }
  ";"					{ \s -> SEMI }
  ">"					{ \s -> GTHAN }
  ">="					{ \s -> GEQUAL }
  "<"              			{ \s -> LTHAN }
  "<="					{ \s -> LEQUAL }
  "="					{ \s -> EQUAL }
  "!="					{ \s -> NEQUAL }
  "+"					{ \s -> PLUS }
  "-"					{ \s -> MINUS }
  "*"					{ \s -> MUL }
  "/"					{ \s -> DIV }
  "!"					{ \s -> NOT }
  "("					{ \s -> LPAREN }
  ")"					{ \s -> RPAREN }
  "{"					{ \s -> LBRACE }
  "}"					{ \s -> RBRACE }
  $digit+				{ \s -> INTLITERAL (read s) }
  "["                   		{ \s -> LBRACKET }
  "]"				   	{ \s -> RBRACKET }
  ";"					{ \s -> COLON }
  "if" 			           	{ \s -> IF }
  "then"				{ \s -> THEN }
  "else"				{ \s -> ELSE }
  "fi"					{ \s -> FI }
  "while" 				{ \s -> WHILE }
  "do" 					{ \s -> DO }
  "od" 					{ \s -> OD }
  "skip" 				{ \s -> SKIP }
  "write" 				{ \s -> WRITE }
  "read" 				{ \s -> READ }
  "program" 				{ \s -> PROGRAM }
  "end" 				{ \s -> END }
  "true" 				{ \s -> TRUE }
  "false" 				{ \s -> FALSE }
  "int" 				{ \s -> INT }
  $alpha[$alpha $digit \_]*		{ \s -> IDENTIFIER s }
  
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    	AND  |
	OR |
	ASSIGN |
	SEMI |
	GTHAN |
	GEQUAL |
	LTHAN |
	LEQUAL |
	EQUAL |
	NEQUAL |
	PLUS	 |
	MINUS |
	MUL |
	DIV |
	NOT |
	LPAREN |
	RPAREN |
	LBRACE |
	RBRACE |
	INTLITERAL Int |
	LBRACKET |
	RBRACKET |
	COLON |
	IF |
	THEN |
	ELSE |
	FI |
    	WHILE |
	DO |
	OD |
	SKIP |
	WRITE |
	READ |
	PROGRAM |
    	END |
	TRUE |
	FALSE |
    	INT |
	IDENTIFIER String
	deriving (Eq,Show)

main = do
	s <- getContents
	print (alexScanTokens s)
}
