%{
	#include <stdio.h>
	extern int yylex(void);
	extern char *yytext;
	extern int yyleng;
	extern int yylineno;
	void yyerror(const char*);
%}

%token MODULE
%token IMPORT FROM AS
%token STR1_LITERAL STR2_LITERAL RSTR1_LITERAL RSTR2_LITERAL
%token HSTR1_LITERAL HSTR2_LITERAL HRSTR1_LITERAL HRSTR2_LITERAL
%token COMMENT
%token DASH_GREATER
%token IDENTIFIER
%token DEF
%token BOOL CHAR BYTE INT INT8 INT16 INT32 INT64 UINT UINT8 UINT16 UINT32 UINT64 FLOAT32 FLOAT64 FLOAT128
%token STRING POINTER
%token TYPE STRUCT UNION ENUM OPTION
%token TRUE FALSE
%token REGISTER STATIC
%token CONST VOLATILE RESTRICT ATOMIC CONST_RESTRICT
%token BINARY_LIT OCTAL_LIT DECIMAL_LIT HEX_LIT FLOAT_LIT CHAR_LIT STRING_LIT
%token RIGHT_SHIFT LEFT_SHIFT RIGHT_SHIFT_US LEFT_SHIFT_US
%token IS_EQUAL IS_NOT_EQUAL IS_LESS IS_GREATER IS_LESS_OR_EQ IS_GREATER_OR_EQ
%token LOGICAL_OR LOGICAL_AND
%token BITWISE_AND BITWISE_OR BITWISE_NOT BITWISE_XOR

%start source_file

%%

source_file	: top_level			{ }
		| source_file top_level		{ }
		;

top_level	: import_decl			{ }
		/* | type_defn			{ } */
		/* | type_func			{ } */
		| module_defn			{ }
		| func_defn			{ }
		| COMMENT			{ printf("Comment\n"); }
		;

import_decl	: IMPORT STR1_LITERAL FROM STR1_LITERAL AS STR1_LITERAL		{ printf("Import From As\n"); }
		| IMPORT STR1_LITERAL FROM STR1_LITERAL				{ printf("Import From\n"); }
		| IMPORT STR1_LITERAL						{ printf("Import\n"); }
		;

module_defn	: MODULE IDENTIFIER '{' '}'					{ printf("Module\n"); }

func_defn	: DEF IDENTIFIER func_sign func_body				{ printf("Function\n"); }
		;

func_sign	: '(' func_param_list ')' DASH_GREATER '(' func_return_list ')'
		;

func_body	: '{' statements '}'
		;

func_param_list	: /* empty */
		| func_param
		| func_param_list ',' func_param
		;

func_return_list : /* empty */
		| func_return
		| func_return_list ',' func_return
		;

func_param	: type IDENTIFIER
		;

func_return	: type IDENTIFIER
		| type
		;

type		: storage_class type_qualifier type_name
		| storage_class type_name
		| type_qualifier type_name
		| type_name
		;

storage_class	: REGISTER				{ printf("Register\n"); }
		| STATIC				{ printf("Static\n"); }
		;

type_qualifier	: CONST					{ printf("Const\n"); }
		| VOLATILE				{ printf("Volatile\n"); }
		| RESTRICT				{ printf("Restirct\n"); }
		| ATOMIC				{ printf("Atomic\n"); }
		| CONST_RESTRICT			{ printf("Const Restrict\n"); }
		;

type_name	: BOOL					{ printf("Bool\n"); }
		| CHAR					{ printf("Char\n"); }
		| BYTE					{ printf("Byte\n"); }
		| INT					{ printf("Int\n"); }
		| INT8					{ printf("Int8\n"); }
		| INT16					{ printf("Int16\n"); }
		| INT32					{ printf("Int32\n"); }
		| INT64					{ printf("Int64\n"); }
		| UINT					{ printf("UInt\n"); }
		| UINT8					{ printf("UInt8\n"); }
		| UINT16				{ printf("UInt16\n"); }
		| UINT32				{ printf("UInt32\n"); }
		| UINT64				{ printf("UInt64\n"); }
		| FLOAT32				{ printf("UInt32\n"); }
		| FLOAT64				{ printf("UInt64\n"); }
		| FLOAT128				{ printf("UInt128\n"); }
		| STRING				{ printf("String\n"); }
		| POINTER ':' type_name			{ printf("Pointer\n"); }
		| IDENTIFIER				{ printf("CustomType\n"); }
		;

statements	: statements stmt
		| stmt
		;

stmt		: var_decl
		| type_defn
/*		| expression_stmt */
		| assignment_stmt			{ printf("Ass Stmt\n"); }
		;

var_decl	: type IDENTIFIER
		| type IDENTIFIER '=' literal
		;

literal		: bool_lit
		| int_lit
		| float_lit
		| char_lit
		| str_lit
/*		| ptr_lit */
		| func_lit
/*		| composite_lit */
		| tupple_lit
		;

bool_lit	: TRUE					{ printf("True\n"); }
		| FALSE					{ printf("False\n"); }
		;

int_lit		: BINARY_LIT				{ printf("Binary Literal\n"); }
		| OCTAL_LIT				{ printf("Octal Literal\n"); }
		| DECIMAL_LIT				{ printf("Decimal Literal\n"); }
		| HEX_LIT				{ printf("Hex Literal\n"); }
		;

float_lit	: FLOAT_LIT				{ printf("Float Literal\n"); }
		;

char_lit	: CHAR_LIT				{ printf("Char Literal\n"); }
		;

str_lit		: STR1_LITERAL				{ printf("String1 Literal\n"); }
		| STR2_LITERAL				{ printf("String2 Literal\n"); }
		| RSTR1_LITERAL				{ printf("RString1 Literal\n"); }
		| RSTR2_LITERAL				{ printf("RString2 Literal\n"); }
		| HSTR1_LITERAL				{ printf("HString1 Literal\n"); }
		| HSTR2_LITERAL				{ printf("HString2 Literal\n"); }
		| HRSTR1_LITERAL			{ printf("HRString1 Literal\n"); }
		| HRSTR2_LITERAL			{ printf("HRString2 Literal\n"); }
		;

type_defn	: struct_defn
		| union_defn
		| enum_defn
		| option_defn
		;

struct_defn	: TYPE STRUCT IDENTIFIER '{' field_decl '}'	{ printf("Struct Defn\n"); }
		;

union_defn	: TYPE UNION IDENTIFIER '{' field_decl '}'	{ printf("Union Defn\n"); }
		;

enum_defn	: TYPE ENUM IDENTIFIER '{' enum_fields '}'	{ printf("Enum Defn\n"); }
		;

option_defn	: TYPE OPTION IDENTIFIER '{' field_decl '}'	{ printf("Option Defn\n"); }
		;

field_decl	: type IDENTIFIER
		| field_decl type IDENTIFIER
		;

enum_fields	: IDENTIFIER
		| enum_fields IDENTIFIER
		;

assignment_stmt	: identifier_list assign_op expression		{ printf("Assign stmt\n"); }
		| identifier_list assign_op IDENTIFIER		{ printf("Assign stmt\n"); }
		;

identifier_list	: IDENTIFIER
		| identifier_list ',' IDENTIFIER
		;

assign_op	: '='						{ printf("=\n"); }
		| arith_op '='					{ printf("Arith =\n"); }
		| shift_op '='					{ printf("Shift =\n"); }
		| logical_op '='				{ printf("Logical =\n"); }
		;

arith_op	: '+'						{ printf("+\n"); }
		| '-'						{ printf("-\n"); }
		| '/'						{ printf("/\n"); }
		| '%'						{ printf("%%\n"); }
		;

shift_op	: RIGHT_SHIFT					{ printf("<<\n"); }
		| LEFT_SHIFT					{ printf(">>\n"); }
		| RIGHT_SHIFT_US				{ printf("<<<\n"); }
		| LEFT_SHIFT_US					{ printf(">>>\n"); }
		;

logical_op	: LOGICAL_AND					{ printf("&&\n"); }
		| LOGICAL_OR					{ printf("||\n"); }
		;

expression	: operand_expr binary_op operand_expr		{ printf("Binary Expr\n"); }
		;

binary_op	: arith_op
		| rel_op
		| shift_op
		| logical_op
		| bit_op
		;

rel_op		: IS_EQUAL					{ printf("==\n"); }
		| IS_NOT_EQUAL					{ printf("!=\n"); }
		| IS_LESS					{ printf("<\n"); }
		| IS_GREATER					{ printf(">\n"); }
		| IS_LESS_OR_EQ					{ printf("<=\n"); }
		| IS_GREATER_OR_EQ				{ printf(">=\n"); }
		;

bit_op		: BITWISE_AND					{ printf("&\n"); }
		| BITWISE_OR					{ printf("|\n"); }
		| BITWISE_NOT					{ printf("^\n"); }
		| BITWISE_XOR					{ printf("&^\n"); }
		;

operand_expr	: primary_expr
		;

primary_expr	: operand
		| primary_expr field_selector
		| primary_expr index
		| primary_expr call
		;

field_selector	: '.' IDENTIFIER				{ printf("X.Y\n"); }
		;

index		: '[' expression ']'				{ printf("[ ]\n"); }
		;

call		: '(' ')'					{ printf("()\n"); }
		| '(' expression_list ')'			{ printf("(...)\n"); }
		;

expression_list	: expression
		| expression_list ',' expression
		;

operand		: literal
		| IDENTIFIER
		| '(' expression ')'
		;

tupple_lit	: '(' tupple_lit_items ')'
		;

tupple_lit_items : tupple_lit_item
		| tupple_lit_item ',' tupple_lit_items
		;

tupple_lit_item	: IDENTIFIER
		| expression
		| literal
		;

func_lit	: DEF func_sign func_body
		;

%%

int main() {
	yyparse();
	return 0;
}

void yyerror(const char *msg) {
	fprintf(stderr, "parser : %s at line %d\n", msg, yylineno);
}
