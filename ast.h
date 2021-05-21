#ifndef AST_H
#define AST_H

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

extern llvm::LLVMContext Context;
extern llvm::IRBuilder<> Builder;
extern std::unique_ptr<llvm::Module> Module;
extern llvm::BasicBlock *BB;

class TopLevel;

class ImportDecl;
class FunctionDefn;
class AccessModifier;
class CompositeTypeDefn;
class TypeAlias;
class TypeFunction;
class FunctionDefnList;
class Block;
class NamespaceDefn;
class NamespaceBlockList;
class NamespaceBlock;

class Type;
class Storage;
class TypeQualifier;
class TypeName;
class Literal;
class BooleanLiteral;
class IntegerLiteral;
class FloatLiteral;
class CharLiteral;
class StringLiteral;
class PointerLiteral;
class CompositeLiteral;

class Statements;
class Statement;
class VariableDef;
class VarIdentExp;
class VarIdentExpList;
class ExpressionStmt;
class AssignmentStmt;
class SelectionStmt;
class IterationStmt;
class JumpStmt;
class DeferStmt;
class LabelStmt;

class IfElseStmt;
class IfBlock;
class ElseBlock;

class SwitchStmt;
class CaseBlock;
class CaseExpressionStmt;

class ForStmt;
class ForInit;
class ForCondition;
class ForPost;
class WhileStmt;
class DoWhileStmt;

class FunctionSign;
class FunctionParam;
class FunctionReturn;

class StructDefn;
class UnionDefn;
class EnumDefn;
class StructUnionFields;
class StructUnionField;
class EnumFields;

class AssignOp;

class LValue;
class LValueList;

class Expression;
class UnaryExpression;
class BinaryExpression;
class Operand;
class NameSpaceIdent;
class Index;
class FunctionCallOp;
class ReturnArgumentList;

class TypeIdentifier;

class SourceFile {
	public:
		std::list<TopLevel *> t;
		void codeGen();
};

class TopLevel {
	public:
		enum types { IMPORT_DECL, VARIABLE_DEF, COMPOSITE_TYPE_DEFN, TYPE_ALIAS, TYPE_FUNC,
			NAMESPACE_DEFN, FUNC_DEFN } type;
		ImportDecl *id;
		VariableDef *vd;
		CompositeTypeDefn *ctd;
		TypeAlias *ta;
		TypeFunction *tf;
		NamespaceDefn *nsd;
		FunctionDefn *fd;
		void codeGen();
};

class ImportDecl {
	public:
		std::string import;
		std::string from;
		std::string as;
		void codeGen();
};

class VariableDef {
	public:
		bool is_global = false;
		VarIdentExpList *v;
		void codeGen();
};

class VarIdentExpList {
	public:
		std::list<VarIdentExp *> viel;
};

class VarIdentExp {
	public:
		std::string ident;
		Type *t;
		Expression *e;
};

class CompositeTypeDefn {
	public:
		enum types { STRUCT, UNION, ENUM } type;
		bool is_global = false;
		StructDefn *s;
		UnionDefn *u;
		EnumDefn *e;
		void codeGen(bool);
};

class TypeAlias {
	public:
		void codeGen();
};

class TypeFunction {
	public:
		FunctionDefnList * fdl;
		void codeGen();
};

class NamespaceDefn {
	public:
		std::string ident;
		NamespaceBlockList *nsbl;
		void codeGen();

};

class FunctionDefn {
	public:
		AccessModifier *am;
		std::string fn;
		FunctionSign *fs;
		Block *b;
		void codeGen();
};

class StructDefn {
	public:
		std::string ident;
		StructUnionFields *f;
		void codeGen(bool);
};

class UnionDefn {
	public:
		std::string ident;
		StructUnionFields *f;
		void codeGen(bool);
};

class EnumDefn {
	public:
		std::string ident;
		EnumFields *f;
		void codeGen(bool);
};

class StructUnionFields {
	public:
		std::list<VariableDef *> vdl;
};

class EnumFields {
	public:
		std::list<std::string> i;
};

class NamespaceBlockList {
	public:
		std::list<NamespaceBlock *> nsbl;
};

class NamespaceBlock {
	public:
		enum types { VARIABLE_DEF, COMPOSITE_TYPE_DEFN, TYPE_ALIAS, TYPE_FUNC,
			NAMESPACE_DEFN, FUNC_DEFN } type;
		VariableDef *vd;
		CompositeTypeDefn *ctd;
		TypeFunction *tf;
		NamespaceDefn *nd;
		FunctionDefn *fd;
		void codeGen();
};

class FunctionSign {
	public:
		FunctionParam *fp;
		FunctionReturn *fr;
};

class FunctionParam {
	public:
		std::list<TypeIdentifier *> fpl;
};

class FunctionReturn {
	public:
		std::list<TypeIdentifier *> frl;
};

class FunctionDefnList {
	public:
		bool is_set = false;
		std::list<FunctionDefn *> fdl;
};

class Storage {
	public:
		enum storages { REGISTER, STATIC } storage;
};

class TypeQualifier {
	public:
		enum type_qualifiers { CONST, VOLATILE, RESTRICT, ATOMIC, CONST_RESTRICT} type_qualifier;
};

class TypeName {
	public:
		enum type_names { BOOL, CHAR, BYTE, INT, INT8, INT16, INT32, INT64, UINT, UINT8,
			UINT16, UINT32, UINT64, FLOAT32, FLOAT64, FLOAT128, STRING, POINTER,
			GENERIC_POINTER, STRUCT, STRUCT_TEMPLATE, UNION, UNION_TEMPLATE, ENUM,
			FUNCTION, CUSTOM, AUTO } type_name;
};

class Type {
	public:
		Storage *storage;
		TypeQualifier *type_qualifier;
		TypeName *type_name;
};

class AccessModifier {
	public:
		enum types { PUBLIC, PRIVATE } type;
};

class Block {
	public:
		Statements *s = NULL;
		void codeGen();
};

class Statements {
	public:
		bool is_set = false;
		std::list<Statement *> s;
		void codeGen();
};

class Statement {
	public:
		enum types { VARIABLE_DEF, COMPOSITE_TYPE_DEFN, TYPE_ALIAS, EXPRESSION, ASSIGNMENT, SELECTION,
			ITERATION, JUMP, DEFER, BLOCK, LABEL } type;
		VariableDef *vds;
		CompositeTypeDefn *ctd;
		TypeAlias *ta;
		ExpressionStmt *es;
		AssignmentStmt *as;
		SelectionStmt *ss;
		IterationStmt *is;
		JumpStmt *js;
		DeferStmt *ds;
		Block *b;
		LabelStmt *ls;
		void codeGen();
};

class ExpressionStmt {
	public:
		void codeGen();
};

class AssignmentStmt {
	public:
		LValueList *lvl;
		AssignOp *ao;
		Expression *e;
		void codeGen();
};

class LValue {
	public:
		enum types { POSTFIX_EXPR, ADDR_OF_UNARY_EXP, PTR_TO_UNARY_EXP, OPERAND_INDEX } type;
		// QualifiedIdent *qi;
		UnaryExpression *ue;
		Operand *o;
		Index *i;
};

class LValueList {
	public:
		std::list<LValue *> lvl;
		void codeGen();
};

class Expression {
	public:
		enum types { UNARY, BINARY } type;
		UnaryExpression *ue;
		BinaryExpression *be;
		void codeGen();
};

class UnaryExpression {
	public:
		enum types { U_NOT, U_2COMP, U_ADD_OF, MULTIPLY_OR_DEREF, PLUS, MINUS, BRACES, OPERAND } type;
		UnaryExpression *ue;
		Expression *e;
		Operand *o;
		void codeGen();
};

class BinaryExpression {
	public:
		enum types { PLUS, MINUS, MULTIPLY_OR_DEREF, DIVIDE, MODULUS, RIGHT_SHIFT, LEFT_SHIFT,
			RIGHT_SHIFT_US, LEFT_SHIFT_US, LOGICAL_AND, LOGICAL_OR, IS_EQUAL, IS_NOT_EQUAL,
			IS_LESS, IS_GREATER, IS_LESS_OR_EQ, IS_GREATER_OR_EQ,
			BITWISE_AND, BITWISE_OR, BITWISE_NOT, BITWISE_XOR } type;
		Expression *le;
		Expression *re;
		void codeGen();
};

class AssignOp {
	public:
		enum types { PLUS_EQUAL_TO, MINUS_EQUAL_TO, MULTIPLY_EQUAL_TO, DIVIDE_EQUAL_TO, MODULUS_EQUAL_TO,
			RIGHT_SHIFT_EQUAL_TO, LEFT_SHIFT_EQUAL_TO, RIGHT_SHIFT_US_EQUAL_TO, LEFT_SHIFT_US_EQUAL_TO,
			LOGICAL_OR_EQUAL_TO, LOGICAL_AND_EQUAL_TO } type;
		bool is_compound = false;
};

class Operand {
	public:
		enum types { LITERAL, QUALIFIED_IDENT, INDEX, FUNCTION_CALL } type;
		Literal *l;
		NameSpaceIdent *nsi;
		Index *i;
		FunctionCallOp *fco;
		void codeGen();
};

class NameSpaceIdent {
	public:
		std::string ident;
		bool is_dot_NameSpaceIdent = false;
		bool is_ptr_NameSpaceIdent = false;
		NameSpaceIdent *dot_NameSpaceIdent;
		NameSpaceIdent *ptr_NameSpaceIdent;
		void codeGen();
};

class Index {
	public:
		Expression *e;
		void codeGen();
};

class FunctionCallOp {
	public:
		bool is_set = false;
		// ExpressionList *el;
		void codeGen();
};

class ReturnArgumentList {
	public:
		bool is_set = false;
		std::list<Expression *> el;
		void codeGen();
};

class SelectionStmt {
	public:
		enum types { IF_ELSE, SWITCH } type;
		IfElseStmt *ies;
		SwitchStmt *ss;
		void codeGen();
};

class IfElseStmt {
	public:
		IfBlock *if_block;
		ElseBlock *else_block;
		void codeGen();
};

class IfBlock {
	public:
		Expression *e;
		Block *b;
		void codeGen();
};

class ElseBlock {
	public:
		bool is_set_if_else;
		IfElseStmt *if_else;
		Block *b;
		void codeGen();
};

class SwitchStmt {
	public:
		Expression *e;
		CaseBlock *c;
		bool is_set_default = false;
		Statements *default_s;
		void codeGen();

};

class CaseBlock {
	public:
		bool is_set = false;
		std::list<CaseExpressionStmt *> case_expression_stmt;
		void codeGen();
};

class CaseExpressionStmt {
	public:
		Expression *e;
		Statements *s;
		void codeGen();
};

class IterationStmt {
	public:
		enum types { FOR, WHILE, DO_WHILE, FOREACH } type;
		ForStmt *f;
		WhileStmt *w;
		DoWhileStmt *dw;
		void codeGen();

};

class ForStmt {
	public:
		ForInit *i;
		ForCondition *c;
		ForPost *p;
		void codeGen();
};

class ForInit {
	public:
		bool is_set = false;
		void codeGen();
};

class ForCondition {
	public:
		bool is_set = false;
		Expression *e;
		void codeGen();
};

class ForPost {
	public:
		bool is_set = false;
		void codeGen();
};

class WhileStmt {
	public:
		Expression *e;
		Block *b;
		void codeGen();
};

class DoWhileStmt {
	public:
		Expression *e;
		Block *b;
		void codeGen();
};

class JumpStmt {
	public:
		enum types { GOTO, CONTINUE, BREAK, RETURN } type;
		std::string goto_ident;
		void codeGen();
};

class DeferStmt {
	public:
		Block *b;
		void codeGen();
};

class LabelStmt {
	public:
		void codeGen();
};

class Literal {
	public:
		enum types { BOOL, INT, FLOAT, CHAR, STRING, POINTER, COMPOSITE } type;
		BooleanLiteral *boolean;
		IntegerLiteral *integer;
		FloatLiteral *floating;
		CharLiteral *character;
		StringLiteral *string;
		PointerLiteral *pointer;
		CompositeLiteral *composite;
};

class BooleanLiteral {
	public:
		enum types { TRUE, FALSE } type;
};

class IntegerLiteral {
	public:
		enum types { BINARY, OCTAL, DECIMAL, HEX } type;
		long value = 0;
		int reg_size = 0;
};

class FloatLiteral {
	public:
		long double value;
		int reg_size = 0;
};

class CharLiteral {
	public:
		char char_literal;
};

class StringLiteral {
	public:
		enum types { STR1, STR2, RSTR1, RSTR2, HSTR1, HSTR2, HRSTR1, HRSTR2 } type;
		std::string string_literal;
};

class PointerLiteral {
	public:
};

class CompositeLiteral {
	public:
};

class TypeIdentifier {
	public:
		Type *t;
		std::string ident;
		llvm::Type *codeGen();
};

class LLVMVariableDef {
	llvm::Type *llvm_type;
	llvm::Value *llvm_val;
};

#endif /* AST_H */
