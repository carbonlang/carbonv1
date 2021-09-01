#include <iostream>

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
#include "llvm/IR/ValueSymbolTable.h"

#include "ast.h"

#define ALERT(str) std::cout << "\033[49;32m" << str << "\033[0m\n"
#define ERROR(str) std::cout << "\033[49;31m" << str << "\033[0m\n"

/* Pointer to global and current function symbol table as stored in LLVM */
llvm::ValueSymbolTable *global_symbol_table_ptr;
llvm::ValueSymbolTable *func_symbol_table_ptr;

llvm::Type* getLLVMType(TypeName *);

void SourceFile::codeGen() {
	std::list<TopLevel *>::iterator tli;
	for (tli = t.begin(); tli != t.end(); tli++) {
		(*tli)->codeGen();
	}
}


void TopLevel::codeGen() {
	/* Save pointer to global symbol table */
	global_symbol_table_ptr = &((*Module).getValueSymbolTable());

	switch (type) {
		case TopLevel::types::IMPORT_DECL : id->codeGen();
			break;
		case TopLevel::types::VARIABLE_DEF : vd->codeGen();
			break;
		case TopLevel::types::COMPOSITE_TYPE_DEFN : ctd->codeGen(true);
			break;
		case TopLevel::types::TYPE_ALIAS : ta->codeGen();
			break;
		case TopLevel::types::TYPE_FUNC : tf->codeGen();
			break;
		case TopLevel::types::NAMESPACE_DEFN : nsd->codeGen();
			break;
		case TopLevel::types::FUNC_DEFN : fd->codeGen();
			break;
	}
}

void ImportDecl::codeGen() {
}

void VariableDef::codeGen() {
	// REFER https://llvm.org/docs/LangRef.html#type-system

	llvm::Type *llvm_type;

	std::list<VarIdentExp *>::iterator viei;
	for (viei = v->viel.begin(); viei != v->viel.end(); viei++) {
		llvm_type = getLLVMType((*viei)->t->type_name);
		if (is_global == true) {
			if (global_symbol_table_ptr->lookup(parent_ns + (*viei)->ident)) {
				ERROR("Global variable '" + parent_ns + (*viei)->ident + "' already defined previously");
			}
			new llvm::GlobalVariable(*Module, llvm_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + (*viei)->ident);
		} else {
			if (func_symbol_table_ptr->lookup(parent_ns + (*viei)->ident)) {
				ERROR("Function variable '" + parent_ns + (*viei)->ident + "' already defined previously");
			}
			new llvm::AllocaInst(llvm_type, 0, parent_ns + (*viei)->ident, BB);
		}
	}

/*
	llvm::Type *llvm_type;
	llvm::Value *val;

	// static = internal global
	// register = no effect
	return;

	if (type->type_name->type_name == TypeName::type_names::BOOL) {
		llvm_type = llvm::Type::getInt8Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			if (lit->boolean->type == BooleanLiteral::types::TRUE) {
				val = llvm::ConstantInt::get(Context, llvm::APInt(8, 1, true));
			} else {
				val = llvm::ConstantInt::get(Context, llvm::APInt(8, 0, true));
			}
			llvm::StoreInst* stinst = new llvm::StoreInst(val, llvm_alloca_inst, false, BB);
		}
	} else if (type->type_name->type_name == TypeName::type_names::CHAR) {
		llvm_type = llvm::Type::getInt8Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(8, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::BYTE) {
		llvm_type = llvm::Type::getInt8Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(8, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::INT) {
		llvm_type = llvm::Type::getInt64Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(64, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::INT8) {
		llvm_type = llvm::Type::getInt8Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(8, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::INT16) {
		llvm_type = llvm::Type::getInt16Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(16, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::INT32) {
		llvm_type = llvm::Type::getInt32Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(32, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::INT64) {
		llvm_type = llvm::Type::getInt64Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(64, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::UINT) {
		llvm_type = llvm::Type::getInt64Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(64, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::UINT8) {
		llvm_type = llvm::Type::getInt8Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(8, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::UINT16) {
		llvm_type = llvm::Type::getInt16Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(16, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::UINT32) {
		llvm_type = llvm::Type::getInt32Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(32, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::UINT64) {
		llvm_type = llvm::Type::getInt64Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantInt::get(Context, llvm::APInt(64, lit->integer->value, true));
		}
	} else if (type->type_name->type_name == TypeName::type_names::FLOAT32) {
		llvm_type = llvm::Type::getFloatTy(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
		if (lit) {
			// val = llvm::ConstantFP::get(Context, llvm::APFloat(lit->floating->value));
		}
	} else if (type->type_name->type_name == TypeName::type_names::FLOAT64) {
		llvm_type = llvm::Type::getDoubleTy(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (type->type_name->type_name == TypeName::type_names::FLOAT128) {
		llvm_type = llvm::Type::getFP128Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (type->type_name->type_name == TypeName::type_names::STRING) {
		llvm_type = llvm::Type::getInt8Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (type->type_name->type_name == TypeName::type_names::POINTER) {
		llvm_type = llvm::Type::getInt64PtrTy(Context, 0);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (type->type_name->type_name == TypeName::type_names::CUSTOM) {
		llvm_type = llvm::Type::getInt64Ty(Context);
		llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	}
	*/
}

void CompositeTypeDefn::codeGen(bool is_global) {
	switch(type) {
		case CompositeTypeDefn::types::STRUCT : s->codeGen(is_global, parent_ns);
			break;
		case CompositeTypeDefn::types::UNION : u->codeGen(is_global, parent_ns);
			break;
		case CompositeTypeDefn::types::ENUM : e->codeGen(is_global, parent_ns);
			break;
		default : ERROR("Error : CompositeTypeDefn");
	}
}

void TypeAlias::codeGen() {
}

void TypeFunction::codeGen() {
}

void NamespaceDefn::codeGen() {
	std::list<NamespaceBlock *>::iterator nsbli;

	for (nsbli = nsbl->nsbl.begin(); nsbli != nsbl->nsbl.end(); nsbli++) {
		switch ((*nsbli)->type) {
			case NamespaceBlock::types::VARIABLE_DEF :
				if (parent_ns == "") {
					(*nsbli)->vd->parent_ns = ident + "__";
				} else {
					(*nsbli)->vd->parent_ns = parent_ns + ident  + "__";
				}
				(*nsbli)->vd->codeGen();
				break;
			case NamespaceBlock::types::COMPOSITE_TYPE_DEFN :
				if (parent_ns == "") {
					(*nsbli)->ctd->parent_ns = ident  + "__";
				} else {
					(*nsbli)->ctd->parent_ns = parent_ns + ident  + "__";
				}
				(*nsbli)->ctd->codeGen(true);
				break;
			case NamespaceBlock::types::TYPE_ALIAS :
				break;
			case NamespaceBlock::types::TYPE_FUNC :
				break;
			case NamespaceBlock::types::NAMESPACE_DEFN :
				if (parent_ns == "") {
					(*nsbli)->nd->parent_ns = ident + "__";
				} else {
					(*nsbli)->nd->parent_ns = parent_ns + ident + "__";
				}
				(*nsbli)->nd->codeGen();
				break;
			case NamespaceBlock::types::FUNC_DEFN :
				if (parent_ns == "") {
					(*nsbli)->fd->parent_ns = ident + "__";
				} else {
					(*nsbli)->fd->parent_ns = parent_ns + ident + "__";
				}
				(*nsbli)->fd->codeGen();
				break;
		}
	}
}

void FunctionDefn::codeGen() {

	llvm::FunctionType *func_type;

	if (fs->fp) {
		/* Function with parameters */
		std::vector<llvm::Type *> func_param_vec;
		std::list<TypeIdentifier *>::iterator tii;
		for (tii = fs->fp->fpl.begin(); tii != fs->fp->fpl.end(); tii++) {
			func_param_vec.push_back(getLLVMType((*tii)->t->type_name));
		}
		llvm::ArrayRef<llvm::Type *> func_param(func_param_vec);

		func_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), func_param, false);
		if (fs->fr) {
		//	/* Function with return parameters */
		//	std::vector<llvm::Type *> func_return_vec;
		//	std::list<TypeIdentifier *>::iterator tii;
		//	for (tii = fs->fr->frl.begin(); tii != fs->fr->frl.end(); tii++) {
		//		func_return_vec.push_back((*tii)->codeGen());
		//	}
		//	llvm::ArrayRef<llvm::Type *> func_return(func_return_vec);
		//	func_type = llvm::FunctionType::get(func_return_vec, func_param, false);
		}
	} else {
		/* Function without parameters */
		func_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), false);
	}

	llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, parent_ns + ident, Module.get());

	BB = llvm::BasicBlock::Create(Context, "", func);

	Builder.SetInsertPoint(BB);

	/* Save pointer to function symbol table */
	func_symbol_table_ptr = func->getValueSymbolTable();

	if (b) {
		b->codeGen();
	}

	/* Set pointer to function symbol table to NULL at end of function */
	func_symbol_table_ptr = NULL;

	verifyFunction(*func);
}

void Block::codeGen() {
	if (s) {
		s->codeGen();
	}
}

void Statements::codeGen() {

	std::list<Statement *>::iterator si;
	for (si = s.begin(); si != s.end(); si++) {
		if ((*si)->type == Statement::types::VARIABLE_DEF) {
			(*si)->vds->codeGen();
		} else if ((*si)->type == Statement::types::COMPOSITE_TYPE_DEFN) {
			(*si)->ctd->codeGen(false);
		} else if ((*si)->type == Statement::types::TYPE_ALIAS) {
			(*si)->ta->codeGen();
		} else if ((*si)->type == Statement::types::EXPRESSION) {
			(*si)->es->codeGen();
		} else if ((*si)->type == Statement::types::ASSIGNMENT) {
			(*si)->as->codeGen();
		} else if ((*si)->type == Statement::types::SELECTION) {
			(*si)->ss->codeGen();
		} else if ((*si)->type == Statement::types::ITERATION) {
			(*si)->is->codeGen();
		} else if ((*si)->type == Statement::types::JUMP) {
			(*si)->js->codeGen();
		} else if ((*si)->type == Statement::types::DEFER) {
			(*si)->ds->codeGen();
		} else if ((*si)->type == Statement::types::BLOCK) {
			/* SEG FAULTS */
			// (*si)->b->codeGen();
		} else if ((*si)->type == Statement::types::LABEL) {
			(*si)->ls->codeGen();
		} else {
			ERROR("Error : Statement type does not exists");
		}
	}
}

void ExpressionStmt::codeGen() {
}

void AssignmentStmt::codeGen() {
	std::list<LValue *>::iterator l_value_iter;
	std::list<Expression *>::iterator expr_iter;

	expr_iter = expr_list_ptr->expr_list.begin();

	llvm::Value *l_val = NULL;
	llvm::Value *expr_val = NULL;

	for (l_value_iter = l_value_list_ptr->l_value_list.begin();
		l_value_iter != l_value_list_ptr->l_value_list.end();
		l_value_iter++) {

		if (expr_iter != expr_list_ptr->expr_list.end()) {
			l_val = (*l_value_iter)->codeGen();
			expr_val = (*expr_iter)->codeGen();
			/* TODO */
			if (l_val && expr_val) {
				Builder.CreateStore(expr_val, l_val, false);
			}
			expr_iter++;
		}
	}
}

llvm::Value * LValue::codeGen() {
	switch (type) {
		case POSTFIX_EXPR :
			return postfix_expr_ptr->codeGen();
			break;
		case PTR_TO_EXP :
			break;
		case UNDERSCORE :
			break;
	}
	return NULL;
}

llvm::Value * Expression::codeGen() {
	switch (type) {
		case Expression::types::UNARY :
			return unary_expr_ptr->codeGen();
			break;
		case Expression::types::BINARY :
			binary_expr_ptr->codeGen();
			break;
		case Expression::types::EXPRESSION :
			expr_ptr->codeGen();
			break;
		default :
			ERROR("Error : Expression type does not exists");
	}
	return NULL;
}

void BinaryExpression::codeGen() {
	left_expr_ptr->codeGen();
	right_expr_ptr->codeGen();
}

llvm::Value * UnaryExpression::codeGen() {
	llvm::Value * val;
	switch (type) {
		case U_NOT :
			val = expr_ptr->codeGen();
			/* TODO : Incorrect : cmp ne 0 -> xor with true -> zext */
			if (val) {
				return Builder.CreateNot(val);
			}
			break;
		case U_COMP :
			val = expr_ptr->codeGen();
			/* TODO : Incorrect */
			if (val) {
				return Builder.CreateXor(val, -1);
			}
			break;
		case U_ADD_OF :
			expr_ptr->codeGen();
			break;
		case STAR :
			expr_ptr->codeGen();
			break;
		case PLUS :
			return expr_ptr->codeGen();
			break;
		case MINUS :
			val = expr_ptr->codeGen();
			if (val) {
				return Builder.CreateNeg(val);
			}
			break;
		case POSTFIX_EXPR :
			// return postfix_expr_ptr->codeGen();
			break;
		case TYPE_CAST :
			return expr_ptr->codeGen();
			break;
		case LITERAL :
			return lit_ptr->codeGen();
			break;
		default :
			ERROR("Error : Unary Expression type does not exists");
	}
	return NULL;
}

llvm::Value * PostfixExpression::codeGen() {
	switch (type) {
		case IDENT_WITH_NS :
			return ident_with_ns_ptr->codeGen();
			break;
		case ARRAY :
			break;
		case FUNCTION_CALL :
			break;
		case DOT_OP :
			break;
		case ARROW_OP :
			break;
	}
	return NULL;
}

llvm::Value * IdentWithNamespace::codeGen() {
	llvm::Value *val = NULL;
	if (func_symbol_table_ptr) {
		val = func_symbol_table_ptr->lookup(ident);
	}
	if (!val) {
		val = global_symbol_table_ptr->lookup(ident);
	}
	if (!val) {
		ERROR("Variable '" + ident + "' not found");
	}
	return val;
}

llvm::Value * Literal::codeGen() {
	switch (type) {
		case BOOL :
			switch (boolean_ptr->type) {
				case BooleanLiteral::types::TRUE :
					return llvm::ConstantInt::getTrue(Context);
				case BooleanLiteral::types::FALSE :
					return llvm::ConstantInt::getFalse(Context);
				default :
					ERROR("Error : Boolean literal type does not exists");
					return NULL;
			}
			break;
		case INT :
			// ALERT(integer_ptr->reg_size);
			return llvm::ConstantInt::get(
				llvm::IntegerType::get(Context, integer_ptr->reg_size),
				integer_ptr->value,
				false);
		case FLOAT :
			// ALERT(floating_ptr->value);
			/* Floating point can be 32, 64, 128 bits wide */
			switch (floating_ptr->reg_size) {
				case 32 :
					return llvm::ConstantFP::get(
						llvm::Type::getFloatTy(Context),
						floating_ptr->value);
				case 64 :
					return llvm::ConstantFP::get(
						llvm::Type::getDoubleTy(Context),
						floating_ptr->value);
				case 128 :
					return llvm::ConstantFP::get(
						llvm::Type::getFP128Ty(Context),
						floating_ptr->value);
				default :
					ERROR("Error : Floating literal size does not exists");
					return NULL;
			}
			break;
		case CHAR :
			/* Integer 8 below represents 8-bits */
			return llvm::ConstantInt::get(
				llvm::IntegerType::get(Context, 8),
				character_ptr->value,
				false);
			break;
		case STRING :
			return llvm::ConstantDataArray::getString(
				Context,
				llvm::StringRef(string_ptr->string_literal),
				true);
			break;
		case POINTER :
			/* TODO */
			return llvm::ConstantPointerNull::get(
				llvm::Type::getInt8PtrTy(Context));
			break;
		case COMPOSITE :
			break;
		default :
			ERROR("Error : Literal type does not exists");
	}
	return NULL;
}

void SelectionStmt::codeGen() {
}

void IterationStmt::codeGen() {
}

void JumpStmt::codeGen() {
}

void DeferStmt::codeGen() {
}

void LabelStmt::codeGen() {
}


void StructDefn::codeGen(bool is_global, std::string parent_ns = "") {
	if (f) {
		std::vector<llvm::Type *> struct_fields_vec;

		std::list<VariableDef *>::iterator vdi;
		for (vdi = f->vdl.begin(); vdi != f->vdl.end(); vdi++) {
			std::list<VarIdentExp *>::iterator viei;
			for (viei = (*vdi)->v->viel.begin(); viei != (*vdi)->v->viel.end(); viei++) {
				struct_fields_vec.push_back(getLLVMType((*viei)->t->type_name));
			}
		}

		llvm::ArrayRef<llvm::Type *> struct_fields(struct_fields_vec);
		llvm::StructType *struct_type = llvm::StructType::create(Module->getContext(), struct_fields, parent_ns + ident);

		if (is_global == true) {
			new llvm::GlobalVariable(*Module, struct_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		} else {
			new llvm::AllocaInst(struct_type, 0, parent_ns + ident, BB);
		}
	} else {
		llvm::ArrayRef<llvm::Type *> struct_fields;
		llvm::StructType *struct_type = llvm::StructType::create(Module->getContext(), struct_fields, parent_ns + ident);

		if (is_global == true) {
			new llvm::GlobalVariable(*Module, struct_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		} else {
			new llvm::AllocaInst(struct_type, 0, parent_ns + ident, BB);
		}
	}
}

void UnionDefn::codeGen(bool is_global, std::string parent_ns = "") {
	if (f) {
		std::vector<llvm::Type *> union_fields_vec;

		std::list<VariableDef *>::iterator vdi;
		for (vdi = f->vdl.begin(); vdi != f->vdl.end(); vdi++) {
			std::list<VarIdentExp *>::iterator viei;
			for (viei = (*vdi)->v->viel.begin(); viei != (*vdi)->v->viel.end(); viei++) {
				union_fields_vec.push_back(getLLVMType((*viei)->t->type_name));
			}
		}

		llvm::ArrayRef<llvm::Type *> union_fields(union_fields_vec);
		llvm::StructType *union_type = llvm::StructType::create(Module->getContext(), union_fields, parent_ns + ident);

		if (is_global == true) {
			new llvm::GlobalVariable(*Module, union_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		} else {
			new llvm::AllocaInst(union_type, 0, parent_ns + ident, BB);
		}
	} else {
		llvm::ArrayRef<llvm::Type *> union_fields;
		llvm::StructType *union_type = llvm::StructType::create(Module->getContext(), union_fields, parent_ns + ident);

		if (is_global == true) {
			new llvm::GlobalVariable(*Module, union_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		} else {
			new llvm::AllocaInst(union_type, 0, parent_ns + ident, BB);
		}
	}
}

void EnumDefn::codeGen(bool is_global, std::string parent_ns = "") {

}

llvm::Type* getLLVMType(TypeName *tn) {

	if (tn->type_name == TypeName::type_names::BOOL) {
		return llvm::Type::getInt8Ty(Context);
	} else if (tn->type_name == TypeName::type_names::CHAR) {
		return llvm::Type::getInt8Ty(Context);
	} else if (tn->type_name == TypeName::type_names::BYTE) {
		return llvm::Type::getInt8Ty(Context);
	} else if (tn->type_name == TypeName::type_names::INT) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::INT8) {
		return llvm::Type::getInt8Ty(Context);
	} else if (tn->type_name == TypeName::type_names::INT16) {
		return llvm::Type::getInt16Ty(Context);
	} else if (tn->type_name == TypeName::type_names::INT32) {
		return llvm::Type::getInt32Ty(Context);
	} else if (tn->type_name == TypeName::type_names::INT64) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UINT) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UINT8) {
		return llvm::Type::getInt8Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UINT16) {
		return llvm::Type::getInt16Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UINT32) {
		return llvm::Type::getInt32Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UINT64) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::FLOAT32) {
		return llvm::Type::getFloatTy(Context);
	} else if (tn->type_name == TypeName::type_names::FLOAT64) {
		return llvm::Type::getDoubleTy(Context);
	} else if (tn->type_name == TypeName::type_names::FLOAT128) {
		return llvm::Type::getFP128Ty(Context);
	} else if (tn->type_name == TypeName::type_names::STRING) {
		return llvm::Type::getInt8Ty(Context);
	} else if (tn->type_name == TypeName::type_names::POINTER) {
		return llvm::Type::getInt64PtrTy(Context, 0);
	} else if (tn->type_name == TypeName::type_names::GENERIC_POINTER) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::STRUCT_TEMPLATE) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::STRUCT) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UNION_TEMPLATE) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::UNION) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::ENUM) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::FUNCTION) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::CUSTOM) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::AUTO) {
		return llvm::Type::getInt64Ty(Context);
	} else {
		ERROR("Error : getLLVMType == NULL");
		return NULL;
	}
}