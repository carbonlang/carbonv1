/*
Copyright (C) 2020 Prashant Shah <pshah.crb@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

%top{
  #include "parser.hh"
%}

%option bison-cc
%option noyywrap
%option namespace="yy"
%option lexer="Lexer"
%option lex="yylex"
%option unicode

bin_digit		[0-1]
oct_digit		[0-7]
dec_digit		[0-9]
hex_digit		[0-9A-F]

bin_lit			("+"|"-")?"2x"{bin_digit}+
oct_lit			("+"|"-")?"8x"{oct_digit}+
dec_lit			("+"|"-")?("10x")?{dec_digit}+
hex_lit			("+"|"-")?"16x"{hex_digit}+

float_exp		("e"|"E")("+"|"-")?{dec_digit}+
float_1			{dec_digit}+"."{dec_digit}*{float_exp}?
float_2			{dec_digit}+{float_exp}
float_3			"."{dec_digit}+{float_exp}
float_lit		("+"|"-")?{float_1}|{float_2}|{float_3}

escape_char		\\("a"|"b"|"f"|"n"|"r"|"t"|"v")
char_lit		"'"([a-zA-z]|{escape_char})"'"|[0-255]

letter			[a-zA-Z]
ident			  {letter}({letter}|{dec_digit})*


%%
"/*".*"*/"				; /* Ignore all comments */

"ns"						  { return yy::parser::token::NAMESAPCE; }
"import"					{ return yy::parser::token::IMPORT; }
"from"						{ return yy::parser::token::FROM; }
"as"              { return yy::parser::token::AS; }

"def"	            { return yy::parser::token::DEF; }

"("							  { return '('; }
")"							  { return ')'; }
"["						   	{ return '['; }
"]"						   	{ return ']'; }
"{"							  { return '{'; }
"}"							  { return '}'; }
"->"						  { return yy::parser::token::FUNC_RETURN; }
"~>"						  { return yy::parser::token::PTR_MEMBER; }
"."							  { return '.'; }
":"							  { return ':'; }
";"							  { return ';'; }
","							  { return ','; }
"..."             { printf("[ ... ]"); }

"register"				{ return yy::parser::token::REGISTER; }
"static"          { return yy::parser::token::STATIC; }
"const"           { return yy::parser::token::CONST; }
"volatile"				{ return yy::parser::token::VOLATILE; }
"restrict"				{ return yy::parser::token::RESTRICT; }
"atomic"          { return yy::parser::token::ATOMIC; }
"const restrict"  { return yy::parser::token::CONST_RESTRICT; }

"bool"						{ return yy::parser::token::BOOL; }
"char"						{ return yy::parser::token::CHAR; }
"byte"						{ return yy::parser::token::BYTE; }
"int"					  	{ return yy::parser::token::INT; }
"int8"						{ return yy::parser::token::INT8; }
"int16"						{ return yy::parser::token::INT16; }
"int32"						{ return yy::parser::token::INT32; }
"int64"						{ return yy::parser::token::INT64; }
"uint"						{ return yy::parser::token::UINT; }
"uint8"						{ return yy::parser::token::UINT8; }
"uint16"					{ return yy::parser::token::UINT16; }
"uint32"					{ return yy::parser::token::UINT32; }
"uint64"					{ return yy::parser::token::UINT64; }
"float32"					{ return yy::parser::token::FLOAT32; }
"float64"					{ return yy::parser::token::FLOAT64; }
"float128"				{ return yy::parser::token::FLOAT128; }

"string"					{ return yy::parser::token::STRING; }

"ptr"   			 		{ return yy::parser::token::POINTER; }

"true"						{ return yy::parser::token::TRUE; }
"false"						{ return yy::parser::token::FALSE; }

"type"						{ return yy::parser::token::TYPE; }
"struct"					{ return yy::parser::token::STRUCT; }
"union"						{ return yy::parser::token::UNION; }
"enum"						{ return yy::parser::token::ENUM; }
"option"					{ return yy::parser::token::OPTION; }

{bin_lit}					{ return yy::parser::token::BINARY_LIT; }
{oct_lit}					{ return yy::parser::token::OCTAL_LIT; }
{dec_lit}					{ return yy::parser::token::DECIMAL_LIT; }
{hex_lit}					{ return yy::parser::token::HEX_LIT; }

{float_lit}				{ return yy::parser::token::FLOAT_LIT; }

{char_lit}				{ return yy::parser::token::CHAR_LIT; }

\"[^"]*\"					{ return yy::parser::token::STR1_LITERAL; }
\'[^']*\'					{ return yy::parser::token::STR2_LITERAL; }
"r"\"[^"]*\"			{ return yy::parser::token::RSTR1_LITERAL; }
"r"\'[^']*\'			{ return yy::parser::token::RSTR2_LITERAL; }

\"\"\"[^"""]*\"\"\"				{ return yy::parser::token::HSTR1_LITERAL; }
\'\'\'[^''']*\'\'\'				{ return yy::parser::token::HSTR2_LITERAL; }
"r"\"\"\"[^"""]*\"\"\"		{ return yy::parser::token::HRSTR1_LITERAL; }
"r"\'\'\'[^''']*\'\'\'		{ return yy::parser::token::HRSTR2_LITERAL; }

"null"						{ return yy::parser::token::PTR_NULL; }

"+"							  { return yy::parser::token::PLUS; }
"-"							  { return yy::parser::token::MINUS; }
"*"							  { return yy::parser::token::MULTIPLY; }
"/"							  { return yy::parser::token::DIVIDE; }
"%"						   	{ return yy::parser::token::MODULUS; }
"=="						  { return yy::parser::token::IS_EQUAL; }
"!="						  { return yy::parser::token::IS_NOT_EQUAL; }
"<"							  { return yy::parser::token::IS_LESS; }
">"							  { return yy::parser::token::IS_GREATER; }
"<="						  { return yy::parser::token::IS_LESS_OR_EQ; }
">="						  { return yy::parser::token::IS_GREATER_OR_EQ; }
">>"						  { return yy::parser::token::RIGHT_SHIFT; }
"<<"						  { return yy::parser::token::LEFT_SHIFT; }
">>>"             { return yy::parser::token::RIGHT_SHIFT_US; }
"<<<"             { return yy::parser::token::LEFT_SHIFT_US; }
"||"						  { return yy::parser::token::LOGICAL_OR; }
"&&"						  { return yy::parser::token::LOGICAL_AND; }
"&"							  { return yy::parser::token::BITWISE_AND; }
"|"							  { return yy::parser::token::BITWISE_OR; }
"^"							  { return yy::parser::token::BITWISE_NOT; }
"&^"						  { return yy::parser::token::BITWISE_XOR; }

"!"							  { return yy::parser::token::U_NOT; }
"~"							  { return yy::parser::token::U_2COMP; }
"$"							  { return yy::parser::token::U_POINTER; }
"@"							  { return yy::parser::token::U_ADD_OF; }
"++"						  { return yy::parser::token::U_INC; }
"--"						  { return yy::parser::token::U_DEC; }

"="							  { return yy::parser::token::EQUAL_TO; }

"public"					{ return yy::parser::token::PUBLIC; }
"private"					{ return yy::parser::token::PRIVATE; }

"return"					{ return yy::parser::token::RETURN; }
"break"						{ return yy::parser::token::BREAK; }
"continue"				{ return yy::parser::token::CONTINUE; }
"goto"						{ return yy::parser::token::GOTO; }
"fallthrough"			{ return yy::parser::token::FALLTHROUGH; }
"if"						  { return yy::parser::token::IF; }
"else"						{ return yy::parser::token::ELSE; }
"for"             { return yy::parser::token::FOR; }
"while"						{ return yy::parser::token::WHILE; }
"do"              { return yy::parser::token::DO; }
"switch"					{ return yy::parser::token::SWITCH; }
"case"						{ return yy::parser::token::CASE; }
"default"					{ return yy::parser::token::DEFAULT; }
"defer"						{ return yy::parser::token::DEFER; }
"extend"					{ return yy::parser::token::EXTEND; }

{ident}						{ yylval.build<std::string>(text()); return yy::parser::token::IDENTIFIER; }

%%
