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
%token FUNC_RETURN
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

%token EQUAL_TO
%token PLUS MINUS MULTIPLY DIVIDE MODULUS
%token RIGHT_SHIFT LEFT_SHIFT RIGHT_SHIFT_US LEFT_SHIFT_US
%token IS_EQUAL IS_NOT_EQUAL IS_LESS IS_GREATER IS_LESS_OR_EQ IS_GREATER_OR_EQ
%token LOGICAL_OR LOGICAL_AND
%token BITWISE_AND BITWISE_OR BITWISE_NOT BITWISE_XOR
%token LU_NOT LU_2COMP LU_ADD_OF RU_INC RU_DEC
%token PTR_MEMBER

%token RETURN BREAK CONTINUE GOTO FALLTHROUGH IF ELSE FOR WHILE DO SWITCH CASE DEFAULT DEFER

%token PUBLIC PRIVATE

%left PLUS MINUS MULTIPLY DIVIDE MODULUS
%left RIGHT_SHIFT LEFT_SHIFT RIGHT_SHIFT_US LEFT_SHIFT_US
%left IS_EQUAL IS_NOT_EQUAL IS_LESS IS_GREATER IS_LESS_OR_EQ IS_GREATER_OR_EQ
%left LOGICAL_OR LOGICAL_AND
%left BITWISE_AND BITWISE_OR BITWISE_NOT BITWISE_XOR
%left U_NOT U_2COMP U_ADD_OF U_POINTER U_INC U_DEC

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

import_decl	: IMPORT STR1_LITERAL FROM STR1_LITERAL AS STR1_LITERAL		{ printf("[Import From As]"); }
		| IMPORT STR1_LITERAL FROM STR1_LITERAL				{ printf("[Import From]"); }
		| IMPORT STR1_LITERAL						{ printf("[Import]"); }
		;

module_defn	: MODULE IDENTIFIER '{' '}'					{ printf("[Module]"); }

func_defn	: DEF IDENTIFIER func_sign block				{ printf("[Function]"); }
		| access_modifier DEF IDENTIFIER func_sign block		{ printf("[PP Function]"); }
		;

access_modifier	: PUBLIC
		| PRIVATE
		;

func_sign	: '(' func_param_list ')' FUNC_RETURN '(' func_return_list ')'
		;

block		: '{' statements '}'
		| '{' '}'
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

type_func	: EXTEND type_name '{' func_defns '}'				{ printf("[Type Function]"); }
		| EXTEND type_name '{' '}'					{ printf("[Type Function]"); }
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

storage_class	: REGISTER				{ printf("[Register]"); }
		| STATIC				{ printf("[Static]"); }
		;

type_qualifier	: CONST					{ printf("[Const]"); }
		| VOLATILE				{ printf("[Volatile]"); }
		| RESTRICT				{ printf("[Restirct]"); }
		| ATOMIC				{ printf("[Atomic]"); }
		| CONST_RESTRICT			{ printf("[Const Restrict]"); }
		;

type_name	: BOOL					{ printf("[Bool]"); }
		| CHAR					{ printf("[Char]"); }
		| BYTE					{ printf("[Byte]"); }
		| INT					{ printf("[Int]"); }
		| INT8					{ printf("[Int8]"); }
		| INT16					{ printf("[Int16]"); }
		| INT32					{ printf("[Int32]"); }
		| INT64					{ printf("[Int64]"); }
		| UINT					{ printf("[UInt]"); }
		| UINT8					{ printf("[UInt8]"); }
		| UINT16				{ printf("[UInt16]"); }
		| UINT32				{ printf("[UInt32]"); }
		| UINT64				{ printf("[UInt64]"); }
		| FLOAT32				{ printf("[UInt32]"); }
		| FLOAT64				{ printf("[UInt64]"); }
		| FLOAT128				{ printf("[UInt128]"); }
		| STRING				{ printf("[String]"); }
		| POINTER ':' type_name			{ printf("[Pointer]"); }
/*		| tupple_type				{ printf("[Tupple]"); } */
/*		| function_type				{ printf("[Function]"); } */
		| IDENTIFIER				{ printf("[CustomType]"); }
		;

/*
function_type	: '(' func_param_type ')' FUNC_RETURN '(' func_ret_type ')'
		;

func_param_type	: type
		| func_param_type ',' type
		;

func_ret_type	: type
		| func_ret_type ',' type
		;
*/

/******************************************************************************************/
/************************************** STATEMENTS ****************************************/
/******************************************************************************************/

statements	: statements stmt
		| stmt
		;

stmt		: var_decl				{ printf("[Var Decl]"); }
		| type_defn				{ printf("[Type Defn]"); }
		| expression_stmt			{ printf("[Expr Stmt]"); }
		| assignment_stmt			{ printf("[Assign Stmt]"); }
		| inc_dec_stmt				{ printf("[Inc Dec Stmt]"); }
		| selection				{ printf("[Selection]"); }
		| iteration				{ printf("[Iteration]"); }
		| jump_stmt				{ printf("[Jump stmt]"); }
		| defer_stmt				{ printf("[Defer Stmt]"); }
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

bool_lit	: TRUE					{ printf("[True]"); }
		| FALSE					{ printf("[False]"); }
		;

int_lit		: BINARY_LIT				{ printf("[Binary Literal]"); }
		| OCTAL_LIT				{ printf("[Octal Literal]"); }
		| DECIMAL_LIT				{ printf("[Decimal Literal]"); }
		| HEX_LIT				{ printf("[Hex Literal]"); }
		;

float_lit	: FLOAT_LIT				{ printf("[Float Literal]"); }
		;

char_lit	: CHAR_LIT				{ printf("[Char Literal]"); }
		;

str_lit		: STR1_LITERAL				{ printf("[String1 Literal]"); }
		| STR2_LITERAL				{ printf("[String2 Literal]"); }
		| RSTR1_LITERAL				{ printf("[RString1 Literal]"); }
		| RSTR2_LITERAL				{ printf("[RString2 Literal]"); }
		| HSTR1_LITERAL				{ printf("[HString1 Literal]"); }
		| HSTR2_LITERAL				{ printf("[HString2 Literal]"); }
		| HRSTR1_LITERAL			{ printf("[HRString1 Literal]"); }
		| HRSTR2_LITERAL			{ printf("[HRString2 Literal]"); }
		;

ptr_lit		: PTR_NULL				{ printf("[Null]"); }
		;

func_lit	: DEF func_sign block
		;

composite_lit	: IDENTIFIER '{' composite_list '}'	{ printf("[Composite Literal]"); }
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

struct_defn	: TYPE STRUCT IDENTIFIER '{' field_decl '}'	{ printf("[Struct Defn]"); }
		;

union_defn	: TYPE UNION IDENTIFIER '{' field_decl '}'	{ printf("[Union Defn]"); }
		;

enum_defn	: TYPE ENUM IDENTIFIER '{' enum_fields '}'	{ printf("[Enum Defn]"); }
		;

option_defn	: TYPE OPTION IDENTIFIER '{' field_decl '}'	{ printf("[Option Defn]"); }
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

arith_op	: PLUS						{ printf("[+]"); }
		| MINUS						{ printf("[-]"); }
		| MULTIPLY					{ printf("[-]"); }
		| DIVIDE					{ printf("[/]"); }
		| MODULUS					{ printf("[%%]"); }
		;

shift_op	: RIGHT_SHIFT					{ printf("[<<]"); }
		| LEFT_SHIFT					{ printf("[>>]"); }
		| RIGHT_SHIFT_US				{ printf("[<<<]"); }
		| LEFT_SHIFT_US					{ printf("[>>>]"); }
		;

logical_op	: LOGICAL_AND					{ printf("[&&]"); }
		| LOGICAL_OR					{ printf("[||]"); }
		;

assign_op	: EQUAL_TO					{ printf("[=]"); }
		| arith_op EQUAL_TO				{ printf("[Arith =]"); }
		| shift_op EQUAL_TO				{ printf("[Shift =]"); }
		| logical_op EQUAL_TO				{ printf("[Logical =]"); }
		;

/******************************************************************************************/
/******************************* ASSIGNMENT STATEMENT *************************************/
/******************************************************************************************/

assignment_stmt	: l_value_list assign_op expression		{ printf("[Assign stmt]"); }
		;

l_value_list	: l_value
		| l_value_list ',' l_value
		;

l_value		: qualified_ident
		| U_ADD_OF operand				{ printf("[@]"); }
		| U_POINTER operand				{ printf("[$]"); }
		| operand index					{ printf("[LHS Array Index]"); }
		;

expression	: unary_expr					{ printf("[Unary Expr]"); }
		| binary_expr					{ printf("[Binary Expr]"); }
		;

binary_expr	: expression PLUS expression			{ printf("[+]"); }
		| expression MINUS expression			{ printf("[-]"); }
		| expression MULTIPLY expression		{ printf("[*]"); }
		| expression DIVIDE expression			{ printf("[/]"); }
		| expression MODULUS expression			{ printf("[%%]"); }
		| expression RIGHT_SHIFT expression		{ printf("[>>]"); }
		| expression LEFT_SHIFT expression		{ printf("[<<]"); }
		| expression RIGHT_SHIFT_US expression		{ printf("[>>>]"); }
		| expression LEFT_SHIFT_US expression		{ printf("[<<<]"); }
		| expression LOGICAL_AND expression		{ printf("[&]"); }
		| expression LOGICAL_OR expression		{ printf("[|]"); }
		| expression IS_EQUAL expression		{ printf("[==]"); }
		| expression IS_NOT_EQUAL expression		{ printf("[!=]"); }
		| expression IS_LESS expression			{ printf("[<]"); }
		| expression IS_GREATER expression		{ printf("[>]"); }
		| expression IS_LESS_OR_EQ expression		{ printf("[<=]"); }
		| expression IS_GREATER_OR_EQ expression	{ printf("[>=]"); }
		| expression BITWISE_AND expression		{ printf("[&&]"); }
		| expression BITWISE_OR expression		{ printf("[||]"); }
		| expression BITWISE_NOT expression		{ printf("[^]"); }
		| expression BITWISE_XOR expression		{ printf("[&^\n"); }
		;

unary_expr	: U_NOT unary_expr				{ printf("[!]"); }
		| U_2COMP unary_expr				{ printf("[~]"); }
		| U_ADD_OF unary_expr				{ printf("[@]"); }
		| U_POINTER unary_expr				{ printf("[$]"); }
		| unary_expr U_INC				{ printf("[++]"); }
		| unary_expr U_DEC				{ printf("[--]"); }
		| operand
		;

operand		: literal
		| qualified_ident
		| operand index					{ printf("[Array Index]"); }
		| operand arguments				{ printf("[Function Call]"); }
		| '(' expression ')'				{ printf("[( )]"); }
		;

qualified_ident	: IDENTIFIER					{ printf("[Identifier]"); }
		| qualified_ident '.' IDENTIFIER		{ printf("[X.Y]"); }
		| qualified_ident PTR_MEMBER IDENTIFIER		{ printf("[X~>Y]"); }
		;

index		: '[' expression ']'
		;

arguments	: '(' ')'
		| '(' expression_list ')'
		;

expression_list	: expression
		| expression_list ',' expression
		;

/******************************************************************************************/
/************************************** STATEMENTS ****************************************/
/******************************************************************************************/

var_decl	: type IDENTIFIER
		| type IDENTIFIER EQUAL_TO literal
		;

inc_dec_stmt	: qualified_ident U_INC
		| qualified_ident U_DEC
		;

expression_stmt	: operand
		;

iteration	: for_stmt						{ printf("[For Stmt]"); }
		| while_stmt						{ printf("[While Stmt]"); }
		| dowhile_stmt						{ printf("[DoWhile Stmt]"); }
		;

for_stmt	: FOR '(' for_init for_cond for_post ')' block
		;

for_init	: ';'
		| simple_stmt ';'
		;

for_cond	: ';'
		| expression ';'
		;

for_post	: /* empty */
		| simple_stmt
		;

simple_stmt	: assignment_stmt
		| inc_dec_stmt
		;

while_stmt	: WHILE '(' expression ')' block
		;

dowhile_stmt	: DO block WHILE '(' expression ')'
		;

defer_stmt	: DEFER block
		;

selection	: if_stmt						{ printf("[If Stmt]"); }
		| switch_stmt						{ printf("[Switch Stmt]"); }
		;

if_stmt		: if_block
		| if_block else_block
		| if_block else_if_block else_block
		;

if_block	: IF '(' expression ')' block
		;

else_block	: ELSE block
		;

else_if_block	: ELSE IF '(' expression ')' block
		| else_if_block ELSE IF '(' expression ')' block
		;

switch_stmt	: SWITCH '(' expression ')' '{' case_block '}'
		| SWITCH '(' expression ')' '{' case_block case_default '}'
		;

case_block	: CASE case_cond ':' statements
		| case_block CASE case_cond ':' statements
		;

case_default	: DEFAULT ':' statements
		;

case_cond	: expression
		;

jump_stmt	: GOTO IDENTIFIER					{ printf("[Goto]"); }
		| CONTINUE						{ printf("[Continue]"); }
		| BREAK							{ printf("[Break]"); }
		| RETURN						{ printf("[Return]"); }
/*		| RETURN expression_list				{ printf("[Return Expr]"); } */
		;

%%


// yydebug = 1;

int main() {
	yyparse();
	return 0;
}

void yyerror(const char *msg) {
	fprintf(stderr, "parser : %s at line %d\n", msg, yylineno);
}
