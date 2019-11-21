%top{
	#include <cstdio>
	#include <iostream>
	#include <iomanip>
	using namespace std;
%}

%class{
%}

%init{
%}

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

raw_str_lit		"r"("'".*"'"|\".*\")
itr_str_lit		("'".*"'"|\".*\")
h_raw_str_lit		"r"("'''".*"'''"|\"\"\".*\"\"\")
h_itr_str_lit		("'''".*"'''"|\"\"\".*\"\"\")
str_lit			{raw_str_lit}|{itr_str_lit}|{h_raw_str_lit}|{h_itr_str_lit}

ptr_lit			"null"|hex_lit|bin_lit

letter			[a-zA-Z]
ident			{letter}({letter}|{dec_digit})*

%%

"/*".*"*/"							{ printf("[ COMMENT ]"); }
"def"								{ printf("[ DEF ]"); }
"main"								{ printf("[ MAIN ]"); }
"import"							{ printf("[ IMPORT ]"); }
"return"							{ printf("[ RETURN ]"); }
"."									{ printf("[ . ]"); }
"->"								{ printf("[ -> ]"); }
":"									{ printf("[ : ]"); }
";"									{ printf("[ ; ]"); }
","									{ printf("[ , ]"); }
"..."								{ printf("[ ... ]"); }
"("									{ printf("[ ( ]"); }
")"									{ printf("[ ) ]"); }
"["									{ printf("[ [ ]"); }
"]"									{ printf("[ ] ]"); }
"{"									{ printf("[ { ]"); }
"}"									{ printf("[ } ]"); }

"register"							{ printf("[ STORAGE_CLASS:register ]"); }
"static"							{ printf("[ STORAGE_CLASS:static ]"); }
"const"								{ printf("[ TYPE_QUALIFIER:const ]"); }
"volatile"							{ printf("[ TYPE_QUALIFIER:volatile ]"); }
"restrict"							{ printf("[ TYPE_QUALIFIER:restrict ]"); }
"atomic"							{ printf("[ TYPE_QUALIFIER:atomic ]"); }

"bool"								{ printf("[ TYPE:bool ]"); }
"char"								{ printf("[ TYPE:char ]"); }
"byte"								{ printf("[ TYPE:byte ]"); }
"int"								{ printf("[ TYPE:int ]"); }
"int8"								{ printf("[ TYPE:int8 ]"); }
"int16"								{ printf("[ TYPE:int16 ]"); }
"int32"								{ printf("[ TYPE:int32 ]"); }
"int64"								{ printf("[ TYPE:int64 ]"); }
"uint"								{ printf("[ TYPE:uint ]"); }
"uint8"								{ printf("[ TYPE:uint8 ]"); }
"uint16"							{ printf("[ TYPE:uint16 ]"); }
"uint32"							{ printf("[ TYPE:uint32 ]"); }
"uint64"							{ printf("[ TYPE:uint64 ]"); }
"float32"							{ printf("[ TYPE:float32 ]"); }
"float64"							{ printf("[ TYPE:float64 ]"); }
"float128"							{ printf("[ TYPE:float128 ]"); }

"string"							{ printf("[ TYPE:string ]"); }

"ptr"								{ printf("[ TYPE:ptr ]"); }

"true"								{ printf("[ LIT:true ]"); }
"false"								{ printf("[ LIT:false ]"); }

"type"								{ printf("[ TYPE:type ]"); }
"struct"							{ printf("[ TYPE:struct ]"); }
"union"								{ printf("[ TYPE:union ]"); }
"enum"								{ printf("[ TYPE:enum ]"); }
"option"							{ printf("[ TYPE:option ]"); }

{bin_lit}							{ printf("[ LIT:binary ]"); }
{oct_lit}							{ printf("[ LIT:octal ]"); }
{dec_lit}							{ printf("[ LIT:decimal ]"); }
{hex_lit}							{ printf("[ LIT:hex ]"); }

{float_lit}							{ printf("[ LIT:float ]"); }

{char_lit}							{ printf("[ LIT:char ]"); }

{str_lit}							{ printf("[ LIT:string ]"); }

"null"|{hex_lit}|{bin_lit}			{ printf("[ LIT:pointer ]"); }

{ident}								{ printf("[ IDENTIFIER ]"); }

"+"									{ printf("[ OP:B:AR:+ ]"); }
"-"									{ printf("[ OP:B:AR:- ]"); }
"*"									{ printf("[ OP:B:AR:* ]"); }
"/"									{ printf("[ OP:B:AR:/ ]"); }
"%"									{ printf("[ OP:B:AR:%% ]"); }
"=="								{ printf("[ OP:B:RL:== ]"); }
"!="								{ printf("[ OP:B:RL:!= ]"); }
"<"									{ printf("[ OP:B:RL:< ]"); }
">"									{ printf("[ OP:B:RL:> ]"); }
"<="								{ printf("[ OP:B:RL:<= ]"); }
">="								{ printf("[ OP:B:RL:>= ]"); }
">>"								{ printf("[ OP:B:SH:>> ]"); }
"<<"								{ printf("[ OP:B:SH:<< ]"); }
">>>"								{ printf("[ OP:B:SH:>>> ]"); }
"<<<"								{ printf("[ OP:B:SH:<<< ]"); }
"||"								{ printf("[ OP:B:LG:|| ]"); }
"&&"								{ printf("[ OP:B:LG:&& ]"); }
"&"									{ printf("[ OP:B:BT:& ]"); }
"|"									{ printf("[ OP:B:BT:| ]"); }
"^"									{ printf("[ OP:B:BT:^ ]"); }
"&^"								{ printf("[ OP:B:BT:&^ ]"); }

"!"									{ printf("[ OP:U:L:! ]"); }
"~"									{ printf("[ OP:U:L:~ ]"); }
"@"									{ printf("[ OP:U:L:@ ]"); }
"++"								{ printf("[ OP:U:R:++ ]"); }
"--"								{ printf("[ OP:U:R:-- ]"); }

"="									{ printf("[ OP:B:AS:= ]"); }

"public"							{ printf("[ ASC_MOD:public ]"); }
"private"							{ printf("[ ASC_MOD:private ]"); }



%%

int main(int argc, char **argv) {
	FILE *fd = stdin;
	if (argc > 1 && (fd = fopen(argv[1], "r")) == NULL) {
		exit(EXIT_FAILURE);
	}

	// create a lexer that consumes a file or reads stdin
	Lexer lexer(fd);

	// here we go!
	lexer.lex();

	if (fd != stdin)
		fclose(fd);
	return 0;
}
