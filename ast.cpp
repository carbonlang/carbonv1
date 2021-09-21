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

	/* llvm::StoreInst* stinst = new llvm::StoreInst(val, llvm_alloca_inst, false, BB); */
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
		} else if ((*si)->type == Statement::types::FUNCTION_CALL) {
			(*si)->fcs->codeGen();
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
			(*si)->b->codeGen();
		} else if ((*si)->type == Statement::types::LABEL) {
			(*si)->ls->codeGen();
		} else {
			ERROR("Error : Statement type does not exists");
		}
	}
}

void FunctionCallStmt::codeGen() {
	llvm::Value *val = postfix_expr_ptr->codeGen();
	if (val) {
		/* Function with parameters */
		std::vector<llvm::Value *> func_arg_vec;
		std::list<FunctionArgument *>::iterator exp_itr;
		for (exp_itr = func_call_op_ptr->func_arg_list_ptr->func_arg_list.begin();
			exp_itr != func_call_op_ptr->func_arg_list_ptr->func_arg_list.end(); exp_itr++) {
			func_arg_vec.push_back((*exp_itr)->expr_ptr->codeGen());
		}
		llvm::Function *func_callee = Module->getFunction(val->getName());
		if (!func_callee) {
			ERROR("Unknown function referenced");
			return;
		}
		Builder.CreateCall(func_callee, func_arg_vec);
	} else {
		ERROR("Error : Function call statement");
	}
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

/* COVERED ALL CASES */
llvm::Value * Expression::codeGen() {
	switch (type) {
		case Expression::types::UNARY :
			return unary_expr_ptr->codeGen();
			break;
		case Expression::types::BINARY :
			return binary_expr_ptr->codeGen();
			break;
		case Expression::types::EXPRESSION :
			return expr_ptr->codeGen();
			break;
		default :
			ERROR("Error : Expression type does not exists");
	}
	return NULL;
}

/* COVERED ALL CASES */
llvm::Value * BinaryExpression::codeGen() {
	llvm::Value *l_exp;
	llvm::Value *r_exp;
	l_exp = left_expr_ptr->codeGen();
	r_exp = right_expr_ptr->codeGen();
	/* TODO */
	if (!l_exp || !r_exp) {
		ERROR("NULL BINARY EXPR");
		return NULL;
	}
	switch (type) {
		case PLUS :
			return Builder.CreateAdd(l_exp, r_exp);
			break;
		case MINUS :
			return Builder.CreateSub(l_exp, r_exp);
			break;
		case STAR :
			return Builder.CreateMul(l_exp, r_exp);
			break;
		case DIVIDE :
			return Builder.CreateUDiv(l_exp, r_exp);
			break;
		case MODULUS :
			return Builder.CreateURem(l_exp, r_exp);
			break;
		case RIGHT_SHIFT :
			return Builder.CreateAShr(l_exp, r_exp);
			break;
		case LEFT_SHIFT :
			return Builder.CreateShl(l_exp, r_exp);
			break;
		case RIGHT_SHIFT_US :
			return Builder.CreateLShr(l_exp, r_exp);
			break;
		case LEFT_SHIFT_US :
			return Builder.CreateShl(l_exp, r_exp);
			break;
		case LOGICAL_AND :
			/* TODO */
			//return Builder.CreateLogicalAnd(l_exp, r_exp);
			// return Builder.CreateICmpNE(l_exp, r_exp);
			// Refer : https://llvm.org/doxygen/IRBuilder_8h_source.html#l01562
			return Builder.CreateSelect(l_exp, r_exp,
				llvm::ConstantInt::getNullValue(r_exp->getType()));
			break;
		case LOGICAL_OR :
			/* TODO */
			//return Builder.CreateLogicalOr(l_exp, r_exp);
			// return Builder.CreateICmpNE(l_exp, r_exp);
			// return Builder.CreateSelect(l_exp,
			//	llvm::ConstantInt::getAllOnesValue(r_exp->getType()),r_exp);
			// Refer : https://llvm.org/doxygen/IRBuilder_8h_source.html#l01568
			return Builder.CreateSelect(l_exp, r_exp,
				llvm::ConstantInt::getNullValue(r_exp->getType()));
			break;
		case IS_EQUAL :
			return Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, l_exp, r_exp);
			break;
		case IS_NOT_EQUAL :
			return Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_NE, l_exp, r_exp);
			break;
		case IS_LESS :
			return Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT, l_exp, r_exp);
			break;
		case IS_GREATER :
			return Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT, l_exp, r_exp);
			break;
		case IS_LESS_OR_EQ :
			return Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, l_exp, r_exp);
			break;
		case IS_GREATER_OR_EQ :
			return Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, l_exp, r_exp);
			break;
		case BITWISE_AND :
			return Builder.CreateAnd(l_exp, r_exp);
			break;
		case BITWISE_OR :
			return Builder.CreateOr(l_exp, r_exp);
			break;
		case BITWISE_XOR :
			return Builder.CreateXor(l_exp, r_exp);
			break;
		case BITWISE_AND_NOT :
			/* TODO */
			return Builder.CreateXor(l_exp, r_exp);
			break;
		default :
			ERROR("Error : Binary Expression type does not exists");
	}
	return NULL;
}

/* COVERED ALL CASES */
llvm::Value * UnaryExpression::codeGen() {
	llvm::Value *val = NULL;
	switch (type) {
		case U_NOT :
			/* TODO : Incorrect : cmp ne 0 -> xor with true -> zext */
			val = expr_ptr->codeGen();
			if (val) {
				return Builder.CreateNeg(val);
			} else {
				ERROR("NULL UNARY EXPR");
			}
			break;
		case U_COMPLEMENT :
			/* TODO */
			val = expr_ptr->codeGen();
			if (val) {
				return Builder.CreateNeg(val);
			} else {
				ERROR("NULL UNARY EXPR");
			}
			break;
		case U_ADD_OF :
			/* TODO */
			val = expr_ptr->codeGen();
			if (val) {
				return Builder.CreateGEP(val,
					llvm::ConstantInt::get(llvm::IntegerType::get(Context, 8), 0, false));
			} else {
				ERROR("NULL UNARY EXPR");
			}
			break;
		case STAR :
			/* TODO */
			val = expr_ptr->codeGen();
			if (val) {
				return Builder.CreateLoad(val);
			} else {
				ERROR("NULL UNARY EXPR");
			}
			break;
		case PLUS :
			val = expr_ptr->codeGen();
			if (val) {
				return val;
			} else {
				ERROR("NULL UNARY EXPR");
			}
			break;
		case MINUS :
			val = expr_ptr->codeGen();
			if (val) {
				return Builder.CreateNeg(val);
			} else {
				ERROR("NULL UNARY EXPR");
			}
			break;
		case POSTFIX_EXPR :
			return postfix_expr_ptr->codeGen();
			break;
		case TYPE_CAST :
			/* TODO */
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
	llvm::Value *val = NULL;
	llvm::Function *callee = NULL;
	switch (type) {
		case IDENT_WITH_NS :
			return ident_with_ns_ptr->codeGen();
			break;
		case ARRAY :
			/* TODO */
			val = postfix_expr_ptr->codeGen();
			if (val) {
				return Builder.CreateGEP(val,
					array_expr_ptr->codeGen());
			} else {
				ERROR("NULL POSTFIX ARRAY EXPR");
			}
			break;
		case FUNCTION_CALL :
			/* TODO */
			val = postfix_expr_ptr->codeGen();
			if (val) {
				callee = Module->getFunction(val->getName().data());
				if (callee) {
					return Builder.CreateCall(callee);
				} else {
					ERROR("NULL POSTFIX FUNCTION CALLEE");
				}
			} else {
				ERROR("NULL POSTFIX FUNCTION CALL");
			}
			break;
		case DOT_OP :
			/* TODO */
			val = postfix_expr_ptr->codeGen();
			if (val) {
				if (val->getType()->isPointerTy()) {
					if (val->getType()->getPointerElementType()->isStructTy()) {
						return Builder.CreateStructGEP(val, 0);
					} else {
						ERROR("POSTFIX DOT_OP EXPR : NOT A STRUCT");
					}
				} else {
					ERROR("POSTFIX DOT_OP EXPR : NOT A STRUCT");
				}
			} else {
				ERROR("NULL POSTFIX DOT_OP EXPR");
			}
			break;
		case ARROW_OP :
			/* TODO */
			val = postfix_expr_ptr->codeGen();
			if (val) {
				if (val->getType()->isPointerTy()) {
					if (val->getType()->getPointerElementType()->isStructTy()) {
						return Builder.CreateStructGEP(val, 0);
					} else {
						ERROR("POSTFIX ARROW_OP EXPR : NOT A STRUCT");
					}
				} else {
					ERROR("POSTFIX ARROW_OP EXPR : NOT A STRUCT");
				}
			} else {
				ERROR("NULL POSTFIX ARROW_OP EXPR");
			}
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
			/* TODO */
			break;
		default :
			ERROR("Error : Literal type does not exists");
	}
	return NULL;
}

void SelectionStmt::codeGen() {
	switch (type) {
		case IF_ELSE :
			ies->codeGen();
			break;
		case SWITCH :
			ss->codeGen();
			break;
		default :
			ERROR("Error : Selection stmt type does not exists");
	}
}

void IfElseStmt::codeGen() {
	// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl05.html

	llvm::Value *cond = if_block->e->codeGen();
	if (!cond) {
		return;
	}

	Builder.CreateICmpNE(cond, Builder.getInt1(0), "ifcond");
	llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(Context, "then", TheFunction);
	llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(Context, "else");
	llvm::BasicBlock *EndIfBB = llvm::BasicBlock::Create(Context, "endif");

	Builder.CreateCondBr(cond, ThenBB, ElseBB);

	Builder.SetInsertPoint(ThenBB);
	if_block->b->codeGen();
	Builder.CreateBr(EndIfBB);
	// ThenBB = Builder.GetInsertBlock();

	if (else_block) {
		TheFunction->getBasicBlockList().push_back(ElseBB);
		Builder.SetInsertPoint(ElseBB);
			if (else_block->is_set_if_else) {
				/* if - else if - block */
			} else {
				/* if - else - block */
				else_block->b->codeGen();
			}
		Builder.CreateBr(EndIfBB);
		// ElseBB = Builder.GetInsertBlock();
	}

	TheFunction->getBasicBlockList().push_back(EndIfBB);
	Builder.SetInsertPoint(EndIfBB);

	//llvm::PHINode *Phi = Builder.CreatePHI(llvm::Type::getInt32Ty(Context), 2, "iftmp");
	//Phi->addIncoming(Builder.getInt32(10), ThenBB);
	//Phi->addIncoming(Builder.getInt32(20), ElseBB);
}

void SwitchStmt::codeGen() {
	ALERT("IN SWITCH");
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