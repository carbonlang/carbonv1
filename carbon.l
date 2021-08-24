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
%option bison-locations
%option noyywrap
%option namespace="yy"
%option lexer="Lexer"
%option lex="yylex"
%option unicode

%{
	// global variable to set a flag to insert ';' if its 1 and token is \n
	int ins_eol = 0;
%}

bin_digit			[0-1]
oct_digit			[0-7]
dec_digit			[0-9]
hex_digit			[0-9A-F]

bin_lit				("+"|"-")?"2x"{bin_digit}+
oct_lit				("+"|"-")?"8x"{oct_digit}+
dec_lit				("+"|"-")?("10x")?{dec_digit}+
hex_lit				("+"|"-")?"16x"{hex_digit}+

float_exp			("e"|"E")("+"|"-")?{dec_digit}+
float_1				{dec_digit}+"."{dec_digit}*{float_exp}?
float_2				{dec_digit}+{float_exp}
float_3				"."{dec_digit}+{float_exp}
float_lit			("+"|"-")?{float_1}|{float_2}|{float_3}

/* escape_char			\\("a"|"b"|"f"|"n"|"r"|"t"|"v") */
/* char_lit			"'"([a-zA-z]|{escape_char})"'" */

letter				[a-zA-Z_]
ident				{letter}({letter}|{dec_digit})*

%x COMMENT

%%

"/*"				{ start(COMMENT); }
<COMMENT>[^*]*			; // eat anything that is not a '*'
<COMMENT>"*"+[^*/]*		; // eat '*'s not followed by a '/'
<COMMENT>"*"+"/"		{ start(INITIAL); }
<COMMENT><<EOF>>		{ std::cerr << "EOF in comment\n"; return 1; }

"//"(.)*			; /* Ignore single line commnet */

[ \t\r]				; /* Ignore all whitespaces */
[\n]				{ if (ins_eol == 1) { ins_eol = 0; return yy::parser::token::EOL; } }

"ns"				{ ins_eol = 0; return yy::parser::token::NAMESAPCE; }
"import"			{ ins_eol = 0; return yy::parser::token::IMPORT; }
"from"				{ ins_eol = 0; return yy::parser::token::FROM; }
"as"				{ ins_eol = 0; return yy::parser::token::AS; }

"def"				{ ins_eol = 0; return yy::parser::token::DEF; }
"public"			{ ins_eol = 0; return yy::parser::token::PUBLIC; }
"private"			{ ins_eol = 0; return yy::parser::token::PRIVATE; }
"extend"			{ ins_eol = 0; return yy::parser::token::EXTEND; }

"register"			{ ins_eol = 0; return yy::parser::token::REGISTER; }
"static"			{ ins_eol = 0; return yy::parser::token::STATIC; }
"const"				{ ins_eol = 0; return yy::parser::token::CONST; }
"volatile"			{ ins_eol = 0; return yy::parser::token::VOLATILE; }
"restrict"			{ ins_eol = 0; return yy::parser::token::RESTRICT; }
"atomic"			{ ins_eol = 0; return yy::parser::token::ATOMIC; }
"const restrict"		{ ins_eol = 0; return yy::parser::token::CONST_RESTRICT; }

"bool"				{ ins_eol = 1; return yy::parser::token::BOOL; }
"char"				{ ins_eol = 1; return yy::parser::token::CHAR; }
"byte"				{ ins_eol = 1; return yy::parser::token::BYTE; }
"int"				{ ins_eol = 1; return yy::parser::token::INT; }
"int8"				{ ins_eol = 1; return yy::parser::token::INT8; }
"int16"				{ ins_eol = 1; return yy::parser::token::INT16; }
"int32"				{ ins_eol = 1; return yy::parser::token::INT32; }
"int64"				{ ins_eol = 1; return yy::parser::token::INT64; }
"uint"				{ ins_eol = 1; return yy::parser::token::UINT; }
"uint8"				{ ins_eol = 1; return yy::parser::token::UINT8; }
"uint16"			{ ins_eol = 1; return yy::parser::token::UINT16; }
"uint32"			{ ins_eol = 1; return yy::parser::token::UINT32; }
"uint64"			{ ins_eol = 1; return yy::parser::token::UINT64; }
"float32"			{ ins_eol = 1; return yy::parser::token::FLOAT32; }
"float64"			{ ins_eol = 1; return yy::parser::token::FLOAT64; }
"float128"			{ ins_eol = 1; return yy::parser::token::FLOAT128; }

"string"			{ ins_eol = 1; return yy::parser::token::STRING; }

"ptr"				{ ins_eol = 0; return yy::parser::token::POINTER; }
"generic"			{ ins_eol = 1; return yy::parser::token::GENERIC_POINTER; }

"auto"				{ ins_eol = 0; return yy::parser::token::AUTO; }

"true"				{ ins_eol = 1; return yy::parser::token::TRUE; }
"false"				{ ins_eol = 1; return yy::parser::token::FALSE; }

"type"				{ ins_eol = 0; return yy::parser::token::TYPE; }
"struct"			{ ins_eol = 0; return yy::parser::token::STRUCT; }
"union"				{ ins_eol = 0; return yy::parser::token::UNION; }
"enum"				{ ins_eol = 0; return yy::parser::token::ENUM; }

"null"				{ ins_eol = 1; return yy::parser::token::PTR_NULL; }

"for"				{ ins_eol = 0; return yy::parser::token::FOR; }
"while"				{ ins_eol = 0; return yy::parser::token::WHILE; }
"do"				{ ins_eol = 0; return yy::parser::token::DO; }
"foreach"			{ ins_eol = 0; return yy::parser::token::FOREACH; }
"in"				{ ins_eol = 0; return yy::parser::token::IN; }

"if"				{ ins_eol = 0; return yy::parser::token::IF; }
"else"				{ ins_eol = 0; return yy::parser::token::ELSE; }
"switch"			{ ins_eol = 0; return yy::parser::token::SWITCH; }
"case"				{ ins_eol = 0; return yy::parser::token::CASE; }
"default"			{ ins_eol = 0; return yy::parser::token::DEFAULT; }

"defer"				{ ins_eol = 0; return yy::parser::token::DEFER; }

"return"			{ ins_eol = 1; return yy::parser::token::RETURN; }
"break"				{ ins_eol = 1; return yy::parser::token::BREAK; }
"continue"			{ ins_eol = 1; return yy::parser::token::CONTINUE; }
"goto"				{ ins_eol = 0; return yy::parser::token::GOTO; }

{bin_lit}			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::BINARY_LIT; }
{oct_lit}			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::OCTAL_LIT; }
{dec_lit}			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::DECIMAL_LIT; }
{hex_lit}			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::HEX_LIT; }

{float_lit}			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::FLOAT_LIT; }

\'[^']*\'			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::CHAR_LIT; }

\"[^"]*\"			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::STR_LITERAL; }
"w"\"[^"]*\"			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::WSTR_LITERAL; }
"u8"\"[^"]*\"			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::U8STR_LITERAL; }
"u16"\"[^"]*\"			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::U16STR_LITERAL; }
"u32"\"[^"]*\"			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::U32STR_LITERAL; }
"r"\"[^"]*\"			{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::RSTR_LITERAL; }

"<:"				{ ins_eol = 0; return yy::parser::token::TEMPLATE_START; }
":>"				{ ins_eol = 0; return yy::parser::token::TEMPLATE_END; }

"("				{ ins_eol = 0; return '('; }
")"				{ ins_eol = 1; return ')'; }
"["				{ ins_eol = 0; return '['; }
"]"				{ ins_eol = 1; return ']'; }
"{"				{ ins_eol = 0; return '{'; }
"}"				{ ins_eol = 1; return '}'; }
"->"				{ ins_eol = 0; return yy::parser::token::ARROW; }
"."				{ ins_eol = 0; return '.'; }
":"				{ ins_eol = 1; return ':'; }
";"				{ ins_eol = 0; return ';'; }
","				{ ins_eol = 0; return ','; }
"_"				{ ins_eol = 0; return '_'; }
"..."				{ ins_eol = 0; }

"+="				{ ins_eol = 0; return yy::parser::token::PLUS_EQUAL_TO; }
"-="				{ ins_eol = 0; return yy::parser::token::MINUS_EQUAL_TO; }
"*="				{ ins_eol = 0; return yy::parser::token::STAR_EQUAL_TO; }
"/="				{ ins_eol = 0; return yy::parser::token::DIVIDE_EQUAL_TO; }
"%="				{ ins_eol = 0; return yy::parser::token::MODULUS_EQUAL_TO; }

">>="				{ ins_eol = 0; return yy::parser::token::RIGHT_SHIFT_EQUAL_TO; }
"<<="				{ ins_eol = 0; return yy::parser::token::LEFT_SHIFT_EQUAL_TO; }
">>>="				{ ins_eol = 0; return yy::parser::token::RIGHT_SHIFT_US_EQUAL_TO; }
"<<<="				{ ins_eol = 0; return yy::parser::token::LEFT_SHIFT_US_EQUAL_TO; }
"||="				{ ins_eol = 0; return yy::parser::token::LOGICAL_OR_EQUAL_TO; }
"&&="				{ ins_eol = 0; return yy::parser::token::LOGICAL_AND_EQUAL_TO; }

"+"				{ ins_eol = 0; return yy::parser::token::PLUS; }
"-"				{ ins_eol = 0; return yy::parser::token::MINUS; }
"*"				{ ins_eol = 0; return yy::parser::token::STAR; }
"/"				{ ins_eol = 0; return yy::parser::token::DIVIDE; }
"%"				{ ins_eol = 0; return yy::parser::token::MODULUS; }
"=="				{ ins_eol = 0; return yy::parser::token::IS_EQUAL; }
"!="				{ ins_eol = 0; return yy::parser::token::IS_NOT_EQUAL; }
"<"				{ ins_eol = 0; return yy::parser::token::IS_LESS; }
">"				{ ins_eol = 0; return yy::parser::token::IS_GREATER; }
"<="				{ ins_eol = 0; return yy::parser::token::IS_LESS_OR_EQ; }
">="				{ ins_eol = 0; return yy::parser::token::IS_GREATER_OR_EQ; }
">>"				{ ins_eol = 0; return yy::parser::token::RIGHT_SHIFT; }
"<<"				{ ins_eol = 0; return yy::parser::token::LEFT_SHIFT; }
">>>"				{ ins_eol = 0; return yy::parser::token::RIGHT_SHIFT_US; }
"<<<"				{ ins_eol = 0; return yy::parser::token::LEFT_SHIFT_US; }
"||"				{ ins_eol = 0; return yy::parser::token::LOGICAL_OR; }
"&&"				{ ins_eol = 0; return yy::parser::token::LOGICAL_AND; }
"&"				{ ins_eol = 0; return yy::parser::token::BITWISE_AND; }
"|"				{ ins_eol = 0; return yy::parser::token::BITWISE_OR; }
"^"				{ ins_eol = 0; return yy::parser::token::BITWISE_NOT; }
"&^"				{ ins_eol = 0; return yy::parser::token::BITWISE_XOR; }

"!"				{ ins_eol = 0; return yy::parser::token::U_NOT; }
"~"				{ ins_eol = 0; return yy::parser::token::U_2COMP; }
"$"				{ ins_eol = 0; return yy::parser::token::U_POINTER; }
"@"				{ ins_eol = 0; return yy::parser::token::U_ADD_OF; }

"="				{ ins_eol = 0; return yy::parser::token::EQUAL_TO; }

"::"				{ ins_eol = 0; return yy::parser::token::SCOPE_RESOLUTION; }

{ident}				{ ins_eol = 1; yylval.build<std::string>(text()); return yy::parser::token::IDENTIFIER; }

%%
