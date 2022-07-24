#include <iostream>
#include <stack>

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

/* List of unlinked basic block for a function. Used in label / goto statements */
std::map<std::string, llvm::BasicBlock *> UnlinkedBBMap;

/* Stack of BasicBlock for break / continue in for, while, do-while */
std::stack<llvm::BasicBlock *> ContinueStack;
std::stack<llvm::BasicBlock *> BreakStack;

/* Structure to hold defer blocks and stack */
struct DeferStackItem {
	llvm::BasicBlock *BB_ptr;
	Block *block_ptr;
};
std::stack<DeferStackItem *> DeferStack;

llvm::Type *func_return_type;
llvm::AllocaInst *func_return_tmp_ptr_variable;

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
	std::list<TypeIdentifier *>::iterator tii;
	func_return_type = NULL;
	func_return_tmp_ptr_variable = NULL;

	if (fs->fp) {
		/* Function with parameters */
		std::vector<llvm::Type *> func_param_vec;
		for (tii = fs->fp->fpl.begin(); tii != fs->fp->fpl.end(); tii++) {
			func_param_vec.push_back(getLLVMType((*tii)->t->type_name));
		}
		llvm::ArrayRef<llvm::Type *> func_param(func_param_vec);

		func_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), func_param, false);
		if (fs->fr) {
			/* Function with return parameters */
			std::vector<llvm::Type *> func_return_vec;
			std::list<TypeIdentifier *>::iterator tii;
			for (tii = fs->fr->frl.begin(); tii != fs->fr->frl.end(); tii++) {
				func_return_vec.push_back(getLLVMType((*tii)->t->type_name));
			}
			func_return_type = func_return_vec.back();
		//	llvm::ArrayRef<llvm::Type *> func_return(func_return_vec);
		//	func_type = llvm::FunctionType::get(func_return_vec, func_param, false);
		}
	} else {
		/* Function without parameters */
		func_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), false);
	}

	llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, parent_ns + ident, Module.get());

	/* Set function argument names */
	llvm::Function::arg_iterator arg_iter;
	if (fs->fp) {
		tii = fs->fp->fpl.begin();
		for (arg_iter = func->arg_begin(); arg_iter != func->arg_end(); arg_iter++, tii++) {
			if (tii == fs->fp->fpl.end()) {
				ERROR("Error : Function arument exceeds more than what is generated");
			}
			arg_iter->setName((*tii)->ident);
		}
	}

	BB = llvm::BasicBlock::Create(Context, "", func);

	Builder.SetInsertPoint(BB);

	/* Save pointer to function symbol table */
	func_symbol_table_ptr = func->getValueSymbolTable();

	/* Initialize UnlinkedBBMap, remove all old entries */
	UnlinkedBBMap.clear();

	/* Check if the ContinueStack, BreakStack & Defer are empty */
	if (!ContinueStack.empty()) {
		ERROR("Error : ContinueStack not empty");
	}
	if (!BreakStack.empty()) {
		ERROR("Error : BreakStack not empty");
	}
	if (!DeferStack.empty()) {
		ERROR("Error : DeferStack not empty");
	}

	if (b) {
		b->codeGen();
	}

	/* Generate defer block code */
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	DeferStackItem *top_defer_stack_item;
	bool defer_present = false;
	while (!DeferStack.empty()) {
		defer_present = true;
		top_defer_stack_item = DeferStack.top();
		top_defer_stack_item->BB_ptr->insertInto(Function);
		Builder.SetInsertPoint(top_defer_stack_item->BB_ptr);
		top_defer_stack_item->block_ptr->codeGen();
		DeferStack.pop();
	}
	if (defer_present) {
		// translates to return *ret_tmp
		// Return the deferenced pointer that is used to store the pointer to return value
		llvm::Value *load_tmp_ptr = Builder.CreateLoad(func_return_tmp_ptr_variable);
		llvm::Value *load_tmp_value = Builder.CreateLoad(load_tmp_ptr);
		Builder.CreateRet(load_tmp_value);
	}

	/* Set pointer to function symbol table to NULL at end of function */
	func_symbol_table_ptr = NULL;

	/* Clear the ContinueStack, BreakStack & Defer stack at end of function. By default it should be empty at this stage */
	while (!ContinueStack.empty()) {
		ContinueStack.pop();
	}
	while (!BreakStack.empty()) {
		BreakStack.pop();
	}
	while (!DeferStack.empty()) {
		DeferStack.pop();
	}

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
			// return Builder.CreateLogicalAnd(l_exp, r_exp);
			// return Builder.CreateICmpNE(l_exp, r_exp);
			// Refer : https://llvm.org/doxygen/IRBuilder_8h_source.html#l01562
			/* From LLVM Souce code */
			assert(r_exp->getType()->isIntOrIntVectorTy(1));
			return Builder.CreateSelect(l_exp, r_exp,
				llvm::ConstantInt::getNullValue(r_exp->getType()));
			break;
		case LOGICAL_OR :
			/* TODO */
			// return Builder.CreateLogicalOr(l_exp, r_exp);
			// return Builder.CreateICmpNE(l_exp, r_exp);
			// return Builder.CreateSelect(l_exp,
			// llvm::ConstantInt::getAllOnesValue(r_exp->getType()),r_exp);
			// Refer : https://llvm.org/doxygen/IRBuilder_8h_source.html#l01568
			/* From LLVM Souce code */
			assert(r_exp->getType()->isIntOrIntVectorTy(1));
			return Builder.CreateSelect(l_exp,
				llvm::ConstantInt::getAllOnesValue(r_exp->getType()), r_exp);
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

	llvm::Value *cmp_value = Builder.CreateICmpNE(cond, Builder.getInt1(0), "ifcond");
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(Context, "then", Function);
	llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(Context, "else");
	llvm::BasicBlock *EndIfBB = llvm::BasicBlock::Create(Context, "endif");

	if (else_block) {
		Builder.CreateCondBr(cmp_value, ThenBB, ElseBB);
	} else {
		Builder.CreateCondBr(cmp_value, ThenBB, EndIfBB);
	}

	Builder.SetInsertPoint(ThenBB);
	if_block->b->codeGen();
	Builder.CreateBr(EndIfBB);
	// ThenBB = Builder.GetInsertBlock();

	if (else_block) {
		Function->getBasicBlockList().push_back(ElseBB);
		Builder.SetInsertPoint(ElseBB);
			if (else_block->is_set_if_else) {
				/* if - else if - block */
				else_block->if_else->codeGen();
			} else {
				/* if - else - block */
				else_block->b->codeGen();
			}
		Builder.CreateBr(EndIfBB);
		// ElseBB = Builder.GetInsertBlock();
	}

	Function->getBasicBlockList().push_back(EndIfBB);
	Builder.SetInsertPoint(EndIfBB);

	//llvm::PHINode *Phi = Builder.CreatePHI(llvm::Type::getInt32Ty(Context), 2, "iftmp");
	//Phi->addIncoming(Builder.getInt32(10), ThenBB);
	//Phi->addIncoming(Builder.getInt32(20), ElseBB);
}

void SwitchStmt::codeGen() {
	llvm::Value *exp_value  = NULL;
	llvm::Value *exp_value_deref;
	llvm::Value *case_expr = NULL;
	llvm::Value *cmp_value_l = NULL;
	llvm::Value *cmp_value_r = NULL;
	std::list<CaseBlock *>::iterator case_block_iter;
	std::list<Expression *>::iterator case_expr_iter;

	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *CaseSwitch = NULL;
	llvm::BasicBlock *NextCaseSwitch = NULL;
	llvm::BasicBlock *EndSwitch = llvm::BasicBlock::Create(Context, "end-switch");

	if (is_set_exp) {
		exp_value = e->codeGen();
		for (case_block_iter = c->case_block_list.begin(); case_block_iter != c->case_block_list.end(); case_block_iter++) {
			if (CaseSwitch) {
				CaseSwitch = NextCaseSwitch;
			} else {
				CaseSwitch = llvm::BasicBlock::Create(Context, "case-switch");
			}

			if (std::next(case_block_iter) == c->case_block_list.end()) {
				/* If last case block then next case block is end of block */
				NextCaseSwitch = EndSwitch;
			} else {
				NextCaseSwitch = llvm::BasicBlock::Create(Context, "case-switch");
			}

			if ((*case_block_iter)->is_default) {
				/* Default case */
				Function->getBasicBlockList().push_back(CaseSwitch);
				Builder.SetInsertPoint(CaseSwitch);

				(*case_block_iter)->block_ptr->codeGen();

				Builder.CreateBr(EndSwitch);
			} else {
				/* Non-default case */
				for (case_expr_iter = (*case_block_iter)->case_expr_list_ptr->expr_list.begin();
					case_expr_iter != (*case_block_iter)->case_expr_list_ptr->expr_list.end(); case_expr_iter++) {
					/* Each case expression */
					case_expr = (*case_expr_iter)->codeGen();
					exp_value_deref = Builder.CreateLoad(exp_value);
					if (cmp_value_l) {
						cmp_value_r = Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, exp_value_deref, case_expr);
						/* TODO. See Logical OR */
						cmp_value_l = Builder.CreateSelect(cmp_value_l,
							llvm::ConstantInt::getAllOnesValue(cmp_value_r->getType()), cmp_value_r);
					} else {
						cmp_value_l = Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, exp_value_deref, case_expr);
					}
				}
				Builder.CreateCondBr(cmp_value_l, CaseSwitch, NextCaseSwitch);

				Function->getBasicBlockList().push_back(CaseSwitch);
				Builder.SetInsertPoint(CaseSwitch);

				(*case_block_iter)->block_ptr->codeGen();

				Builder.CreateBr(EndSwitch);
			}
		}
	}

	Function->getBasicBlockList().push_back(EndSwitch);
	Builder.SetInsertPoint(EndSwitch);
}

void IterationStmt::codeGen() {
	switch (type) {
		case FOR :
			f->codeGen();
			break;
		case WHILE :
			w->codeGen();
			break;
		case DO_WHILE :
			dw->codeGen();
			break;
		case FOREACH :
			break;
		default :
			ERROR("Error : Iteration stmt type does not exists");
	}
}

void ForStmt::codeGen() {
	/* Initial condition section */
	if (i->is_set) {
		switch (i->type) {
			case ForInit::types::ASSIGN_STMT :
				i->assign_stmt_ptr->codeGen();
				break;
			case ForInit::types::VARIABLE_DEF_STMT :
				i->var_def_stmt_ptr->codeGen();
				break;
			default :
				ERROR("Error : For init type does not exists");
		}
	}

	/* Start loop section */
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *ForConditionBB = llvm::BasicBlock::Create(Context, "for-condition", Function);
	llvm::BasicBlock *ForLoopBB = llvm::BasicBlock::Create(Context, "for-loop");
	llvm::BasicBlock *ForEndBB = llvm::BasicBlock::Create(Context, "for-end");

	/* Setup ContinueStack & BreakStack for continue and break stmt */
	ContinueStack.push(ForConditionBB);
	BreakStack.push(ForEndBB);

	Builder.CreateBr(ForConditionBB);

	Builder.SetInsertPoint(ForConditionBB);

	llvm::Value *cond_value;
	llvm::Value *cmp_value;

	if (c->is_set) {
		cond_value = c->e->codeGen();
		if (!cond_value) {
			return;
		}
		cmp_value = Builder.CreateICmpNE(cond_value, Builder.getInt1(0));
		Builder.CreateCondBr(cmp_value, ForLoopBB, ForEndBB);
	} else {
		Builder.CreateBr(ForLoopBB);
	}

	// Start insertion in LoopBB.
	Function->getBasicBlockList().push_back(ForLoopBB);
	Builder.SetInsertPoint(ForLoopBB);
	b->codeGen();

	/* Post loop section */
	if (p->is_set) {
		switch (p->type) {
			case ForPost::types::ASSIGN_STMT :
				p->assign_stmt_ptr->codeGen();
				break;
			default :
				ERROR("Error : For init type does not exists");
		}
	}

	Builder.CreateBr(ForConditionBB);

	Function->getBasicBlockList().push_back(ForEndBB);
	Builder.SetInsertPoint(ForEndBB);

	/* Reset the ContinueStack & BreakStack */
	ContinueStack.pop();
	BreakStack.pop();
}

void WhileStmt::codeGen() {
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *WhileConditionBB = llvm::BasicBlock::Create(Context, "while-condition", Function);
	llvm::BasicBlock *WhileLoopBB = llvm::BasicBlock::Create(Context, "while-loop");
	llvm::BasicBlock *WhileEndBB = llvm::BasicBlock::Create(Context, "while-end");

	/* Setup ContinueStack & BreakStack for continue and break stmt */
	ContinueStack.push(WhileConditionBB);
	BreakStack.push(WhileEndBB);

	Builder.CreateBr(WhileConditionBB);

	Builder.SetInsertPoint(WhileConditionBB);

	// Check while condition
	llvm::Value *cond_value;
	llvm::Value *cmp_value;

	cond_value = e->codeGen();
	if (!cond_value) {
		return;
	}
	cmp_value = Builder.CreateICmpNE(cond_value, Builder.getInt1(0));
	Builder.CreateCondBr(cmp_value, WhileLoopBB, WhileEndBB);

	// Start insertion in LoopBB.
	Function->getBasicBlockList().push_back(WhileLoopBB);
	Builder.SetInsertPoint(WhileLoopBB);
	b->codeGen();

	Builder.CreateBr(WhileConditionBB);

	Function->getBasicBlockList().push_back(WhileEndBB);
	Builder.SetInsertPoint(WhileEndBB);

	/* Reset the ContinueStack & BreakStack */
	ContinueStack.pop();
	BreakStack.pop();
}

void DoWhileStmt::codeGen() {
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *DoWhileLoopBB = llvm::BasicBlock::Create(Context, "do-while-loop", Function);
	llvm::BasicBlock *DoWhileEndBB = llvm::BasicBlock::Create(Context, "do-while-end");

	/* Setup ContinueStack & BreakStack for continue and break stmt */
	ContinueStack.push(DoWhileLoopBB);
	BreakStack.push(DoWhileEndBB);

	Builder.CreateBr(DoWhileLoopBB);

	// Start insertion in LoopBB.
	Builder.SetInsertPoint(DoWhileLoopBB);
	b->codeGen();

	// Check while condition
	llvm::Value *cond_value;
	llvm::Value *cmp_value;

	cond_value = e->codeGen();
	if (!cond_value) {
		return;
	}
	cmp_value = Builder.CreateICmpNE(cond_value, Builder.getInt1(0));
	Builder.CreateCondBr(cmp_value, DoWhileLoopBB, DoWhileEndBB);

	Function->getBasicBlockList().push_back(DoWhileEndBB);
	Builder.SetInsertPoint(DoWhileEndBB);

	/* Reset the ContinueStack & BreakStack */
	ContinueStack.pop();
	BreakStack.pop();
}

void JumpStmt::codeGen() {
	/* For GOTO */
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();
	llvm::Function::BasicBlockListType &bb_list = Function->getBasicBlockList();

	llvm::BasicBlock *UnlinkedBB;
	std::map<std::string, llvm::BasicBlock *>::iterator UnlinkedBB_iter;

	/* For continue */
	llvm::BasicBlock *currentBB;

	/* For return */
	DeferStackItem *top_defer_stack_item;

	llvm::Value *return_value = nullptr;

	switch (type) {
		case GOTO :
			ALERT("GOTO : " + goto_ident);
			/* Check for existing label */
			for (llvm::Function::BasicBlockListType::iterator bb_iter = bb_list.begin(); bb_iter != bb_list.end(); bb_iter++) {
				/* If label found jump to that label by a branch instrucion */
				if (bb_iter->getName().data() == goto_ident) {
					Builder.CreateBr(&(*bb_iter));
					return;
				}
			}

			/* Iterate through the UnlinkedBBMap first to check if same goto statement has created a basic block in advance */
			UnlinkedBB_iter = UnlinkedBBMap.begin();
			while (UnlinkedBB_iter != UnlinkedBBMap.end()) {
				/* If existing unlinked BasicBlock to same named goto is found, jump to it */
				if (UnlinkedBB_iter->first == goto_ident) {
					ALERT("FOUND SAME NAMED UNLINKED GOTO. JUMP.");
					/* Jump to the unlinked BasicBlock */
					UnlinkedBB = UnlinkedBB_iter->second;
					Builder.CreateBr(UnlinkedBB);
					return;
				} else {
					++UnlinkedBB_iter;
				}
			}

			ALERT("CREATE A UNLINKED BASICBLOCK.");
			/* Since existing label not found, and no previous same goto has created a unlinked BasicBlock, create
			 * a new unlinked BasicBlock in advance, add it to UnlinkedBBMap and write a jump instruction to it.
			 * Unlinked BasicBlock is not linked to any function
			 */
			UnlinkedBB = llvm::BasicBlock::Create(Context, goto_ident);
			/* Add unlinked BasicBlock & goto indentifier to UnlinkedBBMap */
			UnlinkedBBMap.insert(std::pair<std::string, llvm::BasicBlock *>(goto_ident, UnlinkedBB));
			/* Jump to the unlinked BasicBlock */
			Builder.CreateBr(UnlinkedBB);
			return;
			break;
		case CONTINUE :
			if (ContinueStack.empty()) {
				ERROR("Error : continue statement not in a loop");
				return;
			}
			currentBB = ContinueStack.top();
			ALERT("CONTINUE : ");
			ALERT(currentBB->getName().data());
			Builder.CreateBr(currentBB);
			return;
			break;
		case BREAK :
			if (BreakStack.empty()) {
				ERROR("Error : break statement not in a loop");
				return;
			}
			currentBB = BreakStack.top();
			ALERT("BREAK : ");
			ALERT(currentBB->getName().data());
			Builder.CreateBr(currentBB);
			return;
			break;
		case  RETURN :
			/* Calculate return values */
			if (return_expr_ptr->expr_list_ptr->expr_list.size() == 0) {
				return_value = nullptr;
			} else if (return_expr_ptr->expr_list_ptr->expr_list.size() == 1) {
				return_value = return_expr_ptr->expr_list_ptr->expr_list.front()->codeGen();
			} else {
				/* TODO : Multiple return */
			}

			if (DeferStack.empty()) {
				 Builder.CreateRet(return_value);
			} else {
				if (func_return_type && !func_return_tmp_ptr_variable) {
					func_return_tmp_ptr_variable = new llvm::AllocaInst(
						llvm::PointerType::get(func_return_type, 0), 0, "return_tmp_ptr_variable", BB);
				}

				// If return_value is a pointer then save the pointer to it as func_return_tmp_ptr_variable
				// else its a literal, then create a variable called return_tmp_literal_value varaible
				// and save the return_value to return_tmp_literal_value and then save the pointer to it
				// as func_return_tmp_ptr_variable
				// This is because we need to save pointer to the return value to the func_return_tmp_ptr_variable
				// and literals dont have a pointer value assocaite with it
				// If its "return" i.e. return void then save nullpointer as literal in the above
				if (!return_value) {
					// "return" i.e. return void, create a pointer literal and store NULL value in it
					llvm::AllocaInst *return_tmp_literal_value = new llvm::AllocaInst(
						llvm::PointerType::get(func_return_type, 0), 0, "return_tmp_literal_value", BB);
					Builder.CreateStore(llvm::ConstantPointerNull::get(llvm::PointerType::get(func_return_type, 0)),
						return_tmp_literal_value, false);
					// int *ptr_var = &var
					// store i32* %1/var, i32** %2/ptr_var
					Builder.CreateStore(return_tmp_literal_value, func_return_tmp_ptr_variable, false);
				} else if (return_value->getType()->getTypeID() == llvm::Type::PointerTyID) {
					// int *ptr_var = &var
					// store i32* %1/var, i32** %2/ptr_var
					Builder.CreateStore(return_value, func_return_tmp_ptr_variable, false);
				} else {
					llvm::AllocaInst *return_tmp_literal_value = new llvm::AllocaInst(
						return_value->getType(), 0, "return_tmp_literal_value", BB);
					Builder.CreateStore(return_value, return_tmp_literal_value, false);
					// int *ptr_var = &var
					// store i32* %1/var, i32** %2/ptr_var
					Builder.CreateStore(return_tmp_literal_value, func_return_tmp_ptr_variable, false);
				}

				top_defer_stack_item = DeferStack.top();
				Builder.CreateBr(top_defer_stack_item->BB_ptr);
			}
			return;
			break;
		default :
			ERROR("Error : Jump statement type does not exists");
	}
}

void DeferStmt::codeGen() {
	/* Push pointer to the entire Block b on stack, code generation will take
	    place at the end of the function in FILO manner
	 */
	DeferStackItem *defer_stack_item = new DeferStackItem();
	defer_stack_item->block_ptr = b;
	defer_stack_item->BB_ptr = llvm::BasicBlock::Create(Context, "defer");
	DeferStack.push(defer_stack_item);
}

void LabelStmt::codeGen() {
	llvm::BasicBlock *LabelBB;
	llvm::Function *Function = Builder.GetInsertBlock()->getParent();

	ALERT("LABEL : " + ident);

	/* Check for existing label */
	llvm::Function::BasicBlockListType &bb_list = Function->getBasicBlockList();
	for (llvm::Function::BasicBlockListType::iterator bb_iter = bb_list.begin(); bb_iter != bb_list.end(); bb_iter++) {
		if (bb_iter->getName().data() == ident) {
			ERROR("Error : Basic block already exists");
			return;
		}
	}

	/* Iterate through the UnlinkedBBMap first to check if any goto statement has created a basic block in advance */
	std::map<std::string, llvm::BasicBlock *>::iterator UnlinkedBB_iter = UnlinkedBBMap.begin();
	while (UnlinkedBB_iter != UnlinkedBBMap.end()) {
		if (UnlinkedBB_iter->first == ident) {
			ALERT("FOUND UNLINKED BB. CREATE LABEL.");
			LabelBB = UnlinkedBB_iter->second;
			LabelBB->insertInto(Function);
			Builder.SetInsertPoint(LabelBB);
			/* Remove from UnlinkedBBMap since label generated */
			UnlinkedBBMap.erase(UnlinkedBB_iter++);
			return;
		} else {
			++UnlinkedBB_iter;
		}
	}

	/* Insert basic block after label */
	LabelBB = llvm::BasicBlock::Create(Context, ident, Function);
	Builder.SetInsertPoint(LabelBB);
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
		// llvm::StructType *struct_type = llvm::StructType::create(Module->getContext(), struct_fields, parent_ns + ident);
		llvm::StructType::create(Module->getContext(), struct_fields, parent_ns + ident);

		// Struct type does not get created if no variable is using it
		// if (is_global == true) {
		//	new llvm::GlobalVariable(*Module, struct_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		// } else {
		//	new llvm::AllocaInst(struct_type, 0, parent_ns + ident, BB);
		// }
	} else {
		llvm::ArrayRef<llvm::Type *> struct_fields;
		// llvm::StructType *struct_type = llvm::StructType::create(Module->getContext(), struct_fields, parent_ns + ident);
		llvm::StructType::create(Module->getContext(), struct_fields, parent_ns + ident);

		// Struct type does not get created if no variable is using it
		// if (is_global == true) {
		//	//new llvm::GlobalVariable(*Module, struct_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		// } else {
		//	new llvm::AllocaInst(struct_type, 0, parent_ns + ident, BB);
		// }
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
		// llvm::StructType *union_type = llvm::StructType::create(Module->getContext(), union_fields, parent_ns + ident);
		llvm::StructType::create(Module->getContext(), union_fields, parent_ns + ident);

		// Union type does not get created if no variable is using it
		// if (is_global == true) {
		//	new llvm::GlobalVariable(*Module, union_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		// } else {
		//	new llvm::AllocaInst(union_type, 0, parent_ns + ident, BB);
		// }
	} else {
		llvm::ArrayRef<llvm::Type *> union_fields;
		// llvm::StructType *union_type = llvm::StructType::create(Module->getContext(), union_fields, parent_ns + ident);
		llvm::StructType::create(Module->getContext(), union_fields, parent_ns + ident);

		// Union type does not get created if no variable is using it
		// if (is_global == true) {
		//	new llvm::GlobalVariable(*Module, union_type, false, llvm::GlobalValue::ExternalLinkage, 0, parent_ns + ident);
		// } else {
		//	new llvm::AllocaInst(union_type, 0, parent_ns + ident, BB);
		// }
	}
}

void EnumDefn::codeGen(bool is_global, std::string parent_ns = "") {

}

llvm::Type* getLLVMType(TypeName *tn) {
	// If the type is array then calculate the size of the array
	llvm::Value *array_size_llvm_value;
	uint64_t array_size;
	llvm::ConstantInt* CI;
	if (tn->is_array == true) {
		if (tn->array_expr_ptr->expr_ptr_list.size() == 1) {
			calculated_array_size = tn->array_expr_ptr->expr_ptr_list.front()->codeGen();
			if (CI = llvm::dyn_cast<llvm::ConstantInt>(calculated_array_size)) {
				// foo indeed is a ConstantInt, we can use CI here
			}
			else {
				//CI = 10;
			// foo was not actually a ConstantInt
			}
		} else {
			std::list<Expression *>::iterator expi;
			for (expi = tn->array_expr_ptr->expr_ptr_list.begin();
				expi != tn->array_expr_ptr->expr_ptr_list.end(); expi++) {
				//return Builder.CreateMul((*expi)->codeGen(), (*expi)->codeGen());
			}
		}
	}

	if (tn->type_name == TypeName::type_names::BOOL) {
		if (tn->is_array == false) {
			return llvm::Type::getInt8Ty(Context);
		} else {
			return llvm::ArrayType::get(
				llvm::Type::getInt8Ty(Context), CI->getZExtValue()
			);
		}
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
	} else if (tn->type_name == TypeName::type_names::VOID) {
		return llvm::Type::getInt64Ty(Context);
	} else if (tn->type_name == TypeName::type_names::STRUCT_TEMPLATE) {
		// In new version of LLVM it is in Context */
		if (llvm::StructType::getTypeByName(Context, tn->type_ident)) {
			return llvm::StructType::getTypeByName(Context, tn->type_ident);
		} else {
			ERROR(tn->type_ident);
			ERROR("Error : struct type not found");
			return NULL;
		}
	} else if (tn->type_name == TypeName::type_names::STRUCT) {
		// In new version of LLVM it is in Context */
		if (llvm::StructType::getTypeByName(Context, tn->type_ident)) {
			return llvm::StructType::getTypeByName(Context, tn->type_ident);
		} else {
			ERROR(tn->type_ident);
			ERROR("Error : struct type not found");
			return NULL;
		}
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
		return llvm::Type::getInt32Ty(Context);
		// TODO : 02.06.2022
		// ERROR("Error : getLLVMType == NULL");
		// return NULL;
	}
}