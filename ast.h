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
class FunctionLiteral;
class CompositeLiteral;

class SourceFile {
};

class TopLevel {
	public:
		llvm::Value* codeGen(llvm::LLVMContext& context, llvm::IRBuilder<>& builder);
};

class ImportDecl {

};

class CompositeTypeDefn {

};

class StructDefn {

};

class EnumDefn {

};

class UnionDefn {

};

class OptionDefn {

};

class NamespaceDefn {

};

class FunctionDefn {

};

class FunctionSign {

};

class FunctionParamList {

};

class FunctionParam {

};

class FunctionReturnList {

};

class FunctionReturn {

};

class TypeFunction {

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
			UINT16, UINT32, UINT64, FLOAT32, FLOAT64, FLOAT128, STRING, POINTER, CUSTOM } type_name;
};

class Type {
	public:
		Storage *storage;
		TypeQualifier *type_qualifier;
		TypeName *type_name;
};

class Identifier {
	public:
		std::string name;
};

class AccessModifier {

};

class Block {

};

class Statements {

};

class Statement {

};

class VarDecl {
	public:
		std::string ident;
		Type *type;
		Literal *lit;
};

class ExpressionStmt {

};

class AssignmentStmt {

};

class LValueList {

};

class Expression {

};

class UnaryExpression {

};

class BinaryExpression {

};

class ArithOp {

};

class ShiftOp {

};

class LogicalOp {

};

class AssignOp {

};

class Operand {

};

class QualifiedIdent {

};

class ExpressionList {

};

class SelectionStmt {

};

class IfElseStmt {

};

class SwitchStmt {

};

class CaseStmt {

};

class IterationStmt {

};

class ForStmt {

};

class WhileStmt {

};

class DoWhileStmt {

};

class JumpStmt {

};

class GotoStmt {

};

class ContinueStmt {

};

class BreakStmt {

};

class ReturnStmt {

};

class DeferStmt {

};

class Literal {
	public:
		enum types { BOOL, INT, FLOAT, CHAR, STRING, POINTER, FUNCTION, COMPOSITE } type;
		BooleanLiteral *boolean;
		IntegerLiteral *integer;
		FloatLiteral *floating;
		CharLiteral *character;
		StringLiteral *string;
		PointerLiteral *pointer;
		FunctionLiteral *function;
		CompositeLiteral *composite;
};

class BooleanLiteral {
	public:
		enum types { TRUE, FALSE } type;
};

class IntegerLiteral {
	public:
		enum types { BINARY, OCTAL, DECIMAL, HEX } type;
		int integer_literal;
};

class FloatLiteral {
	public:
		double float_literal;
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

class FunctionLiteral {
	public:
};

class CompositeLiteral {
	public:
};

#endif /* AST_H */
