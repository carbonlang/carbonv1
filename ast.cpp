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

#include "ast.h"

#define DEBUG(str) std::cout << "\n" \
	<< "**********************************************" << "\n" \
	<< "                   " << str << "\n" \
	<< "**********************************************" << "\n";

void SourceFile::codeGen() {
	std::list<TopLevel *>::iterator tli;
	for (tli = t.begin(); tli != t.end(); ++tli) {
			(*tli)->codeGen();
	}
}

void TopLevel::codeGen() {
	if (type == TopLevel::types::FUNC_DEFN) {
		fd->codeGen();
	}
}

void FunctionDefn::codeGen() {
	if (b) {
		b->codeGen();
	}
}

void Block::codeGen() {
	if (s) {
		s->codeGen();
	}
}

void Statements::codeGen() {
	std::list<Statement *>::iterator si;
	for (si = s.begin(); si != s.end(); ++si) {
		if ((*si)->type == Statement::types::VAR_DECL) {
			(*si)->vds->codeGen();
		}
	}
}

void VarDeclStmt::codeGen() {
	// REFER https://llvm.org/docs/LangRef.html#type-system
	llvm::Type *llvm_type;
	llvm::Value *val;

	// static = internal global
	// register = no effect

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

	//llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	//if (lit) {
	//	llvm::StoreInst* stinst = new llvm::StoreInst(val, llvm_alloca_inst, false, BB);
	//}

	// if (lit) {
	// 	// BOOL, INT, FLOAT, CHAR, STRING, POINTER, FUNCTION, COMPOSITE
	// 		if (lit->type == Literal::types::BOOL) {
	// 			if (type->type_name->type_name != TypeName::type_names::BOOL) {
	// 				// ERROR
	// 			}
	// 			if (lit->boolean->type == BooleanLiteral::types::TRUE) {
	// 				num = llvm::ConstantInt::get(Context, llvm::APInt(8, 1, true));
	// 			} else {
	// 				num = llvm::ConstantInt::get(Context, llvm::APInt(8, 0, true));
	// 			}
	// 			llvm::StoreInst* stinst = new llvm::StoreInst(num, llvm_alloca_inst, false, BB);
	// 		} else if (lit->type == Literal::types::INT) {
	// 			if (type->type_name->type_name != TypeName::type_names::INT) {
	// 				num = llvm::ConstantInt::get(Context, llvm::APInt(8, 1, true));
	// 			} else
	// 					type->type_name->type_name != TypeName::type_names::INT8 ||
	// 					type->type_name->type_name != TypeName::type_names::INT16 ||
	// 					type->type_name->type_name != TypeName::type_names::INT32 ||
	// 					type->type_name->type_name != TypeName::type_names::INT64 ||
	// 					type->type_name->type_name != TypeName::type_names::UINT ||
	// 					type->type_name->type_name != TypeName::type_names::UINT8 ||
	// 					type->type_name->type_name != TypeName::type_names::UINT16 ||
	// 					type->type_name->type_name != TypeName::type_names::UINT32 ||
	// 					type->type_name->type_name != TypeName::type_names::UINT64) {
	// 				// ERROR
	// 			}
	// 			num = llvm::ConstantInt::get(Context, llvm::APInt(8, 1, true));
	// 			llvm::StoreInst* stinst = new llvm::StoreInst(num, llvm_alloca_inst, false, BB);
	// 		}
	// }

	//llvm::Type *type = llvm::Type::getInt32Ty(Context);
	//llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(type, 0, ident, BB);
}
