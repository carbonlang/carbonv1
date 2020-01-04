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
%token DASH_GREATER
%token IDENTIFIER
%token DEF
%token BOOL CHAR BYTE INT INT8 INT16 INT32 INT64 UINT UINT8 UINT16 UINT32 UINT64 FLOAT32 FLOAT64 FLOAT128
%token STRING POINTER
%token TYPE STRUCT UNION ENUM OPTION
%token EXTEND
%token TRUE FALSE
%token PTR_NULL
%token REGISTER STATIC
%token CONST VOLATILE RESTRICT ATOMIC CONST_RESTRICT
%token BINARY_LIT OCTAL_LIT DECIMAL_LIT HEX_LIT FLOAT_LIT CHAR_LIT

%token PLUS MINUS MULTIPLY DIVIDE MODULUS
%token RIGHT_SHIFT LEFT_SHIFT RIGHT_SHIFT_US LEFT_SHIFT_US
%token IS_EQUAL IS_NOT_EQUAL IS_LESS IS_GREATER IS_LESS_OR_EQ IS_GREATER_OR_EQ
%token LOGICAL_OR LOGICAL_AND
%token BITWISE_AND BITWISE_OR BITWISE_NOT BITWISE_XOR
%token LU_NOT LU_2COMP LU_ADD_OF RU_INC RU_DEC

%token RETURN BREAK CONTINUE GOTO FALLTHROUGH IF ELSE FOR WHILE DO SWITCH CASE DEFER

%left PLUS MINUS MULTIPLY DIVIDE MODULUS
%left RIGHT_SHIFT LEFT_SHIFT RIGHT_SHIFT_US LEFT_SHIFT_US
%left IS_EQUAL IS_NOT_EQUAL IS_LESS IS_GREATER IS_LESS_OR_EQ IS_GREATER_OR_EQ
%left LOGICAL_OR LOGICAL_AND
%left BITWISE_AND BITWISE_OR BITWISE_NOT BITWISE_XOR
%left LU_NOT LU_2COMP LU_ADD_OF RU_INC RU_DEC

%start source_file

%%

source_file	: top_level			{ }
		| source_file top_level		{ }
		;

top_level	: import_decl			{ }
		| type_defn			{ }
		| type_func			{ }
		| module_defn			{ }
		| func_defn			{ }
		;

import_decl	: IMPORT STR1_LITERAL FROM STR1_LITERAL AS STR1_LITERAL		{ printf("Import From As\n"); }
		| IMPORT STR1_LITERAL FROM STR1_LITERAL				{ printf("Import From\n"); }
		| IMPORT STR1_LITERAL						{ printf("Import\n"); }
		;

module_defn	: MODULE IDENTIFIER '{' '}'					{ printf("Module\n"); }

func_defn	: DEF IDENTIFIER func_sign block				{ printf("Function\n"); }
		;

func_sign	: '(' func_param_list ')' DASH_GREATER '(' func_return_list ')'
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

type_func	: EXTEND type_name '{' func_defns '}'				{ printf("Type Function\n"); }
		| EXTEND type_name '{' '}'
		;

func_defns	: func_defn
		| func_defns func_defn
		;

/******************************************************************************************/
/************************************** TYPES *********************************************/
/******************************************************************************************/

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
/*		| tupple_type				{ printf("Tupple\n"); } */
/*		| function_type				{ printf("Function\n"); } */
		| IDENTIFIER				{ printf("CustomType\n"); }
		;

/*
function_type	: func_sign
		;
*/


/******************************************************************************************/
/************************************** STATEMENTS ****************************************/
/******************************************************************************************/

statements	: statements stmt
		| stmt
		;

stmt		: var_decl
		| type_defn
		| expression_stmt
		| assignment_stmt			{ printf("Assign Stmt\n"); }
		| inc_dec_stmt				{ printf("Inc Dec Stmt\n"); }
		| selection				{ printf("Selection\n"); }
		| iteration				{ printf("Iteration\n"); }
		| jump_stmt				{ printf("Jump stmt\n"); }
		| defer_stmt				{ printf("Defer Stmt\n"); }
		;

var_decl	: type IDENTIFIER
		| type IDENTIFIER '=' literal
		;

block		: '{' statements '}'

expression_stmt	: expression
		;

/******************************************************************************************/
/************************************** LITERAL *******************************************/
/******************************************************************************************/

literal		: bool_lit
		| int_lit
		| float_lit
		| char_lit
		| str_lit
		| ptr_lit
		| func_lit
		| composite_lit
		/* | tupple_lit */
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

ptr_lit		: PTR_NULL				{ printf("Null\n"); }
		;

func_lit	: DEF func_sign block
		;

composite_lit	: IDENTIFIER '{' composite_list '}'	{ printf("Composite Literal\n"); }
		;

composite_list	: keyed_element
		| composite_list ',' keyed_element
		;

keyed_element	: comp_element
		| comp_key ':' comp_element
		;

comp_key	: IDENTIFIER
		;

comp_element	: literal
		;

/*
tupple_lit	: '(' tupple_items ')'
		;

tupple_items	: tupple_item
		| tupple_items ',' tupple_item
		;

tupple_item	: IDENTIFIER
		| literal
		| expression
		;
*/

/******************************************************************************************/
/******************************* COMPOSITE TYPE DEFINITION ********************************/
/******************************************************************************************/

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

/******************************************************************************************/
/************************************** OPERATORS *****************************************/
/******************************************************************************************/

binary_op	: arith_op
		| rel_op
		| shift_op
		| logical_op
		| bit_op
		;

arith_op	: PLUS						{ printf("+\n"); }
		| MINUS						{ printf("-\n"); }
		| MULTIPLY					{ printf("-\n"); }
		| DIVIDE					{ printf("/\n"); }
		| MODULUS					{ printf("%%\n"); }
		;

shift_op	: RIGHT_SHIFT					{ printf("<<\n"); }
		| LEFT_SHIFT					{ printf(">>\n"); }
		| RIGHT_SHIFT_US				{ printf("<<<\n"); }
		| LEFT_SHIFT_US					{ printf(">>>\n"); }
		;

logical_op	: LOGICAL_AND					{ printf("&&\n"); }
		| LOGICAL_OR					{ printf("||\n"); }
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

assign_op	: '='						{ printf("=\n"); }
		| arith_op '='					{ printf("Arith =\n"); }
		| shift_op '='					{ printf("Shift =\n"); }
		| logical_op '='				{ printf("Logical =\n"); }
		;

/******************************************************************************************/
/************************************** STATEMENTS ****************************************/
/******************************************************************************************/

assignment_stmt	: identifier_list assign_op expression		{ printf("Assign stmt\n"); }
		;

identifier_list	: IDENTIFIER
		| identifier_list ',' IDENTIFIER
		;

expression	: unary_expr					{ printf("Unary Expr\n"); }
		| expression binary_op expression		{ printf("Binary Expr\n"); }
		;

unary_expr	: lu_op unary_expr
		| unary_expr ru_op
		| primary_expr
		;

lu_op		: LU_NOT
		| LU_2COMP
		| LU_ADD_OF
		;

ru_op		: RU_INC					{ printf("++\n"); }
		| RU_DEC					{ printf("--\n"); }
		;

primary_expr	: literal
		| IDENTIFIER
		| '(' expression ')'
		;

inc_dec_stmt	: unary_expr ru_op
		;

iteration	: for_stmt
		| while_stmt
		| dowhile_stmt
		;

for_stmt	: FOR '(' for_init for_cond for_post ')' block
		;

for_init	: simnple_stmt ';'
		| ';'
		;

for_cond	: expression ';'
		| ';'
		;

for_post	: simnple_stmt ';'
		| /* empty */
		;

simnple_stmt	: inc_dec_stmt
		| assignment_stmt
		;

while_stmt	: WHILE '(' while_cond ')' block
		;

while_cond	: expression
		;

dowhile_stmt	: DO block WHILE '(' while_cond ')'
		;

defer_stmt	: DEFER block
		;

selection	: if_stmt
		| switch_stmt
		;

if_stmt		: if_block
		| if_block else_block
		| if_block else_if_block else_block
		;

if_block	: IF '(' if_cond ')' block
		;

else_block	: ELSE block
		;

else_if_block	: ELSE IF '(' if_cond ')' block
		| else_if_block ELSE IF '(' if_cond ')' block
		;

if_cond		: expression
		;

switch_stmt	: SWITCH '(' case_cond ')' '{' case_block '}'
		;

case_block	: CASE case_cond ':' statements
		| case_block CASE ':' statements
		;

case_cond	: expression
		;

jump_stmt	: GOTO IDENTIFIER
		| CONTINUE
		| BREAK
		| RETURN
		;

%%

int main() {
	yyparse();
	return 0;
}

void yyerror(const char *msg) {
	fprintf(stderr, "parser : %s at line %d\n", msg, yylineno);
}
