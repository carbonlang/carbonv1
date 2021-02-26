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

#define ALERT(str) std::cout << "\033[42;31m" << str << "\033[0m\n"

void SourceFile::codeGen() {
	std::list<TopLevel *>::iterator tli;
	for (tli = t.begin(); tli != t.end(); ++tli) {
			(*tli)->codeGen();
	}
}

void TopLevel::codeGen() {
	if (type == TopLevel::types::FUNC_DEFN) {
		fd->codeGen();
	} else if (type == TopLevel::types::COMPOSITE_TYPE_DEFN) {
		ctd->codeGen();
	}
}

void FunctionDefn::codeGen() {

	llvm::FunctionType *func_type;

	if (fs->fp->is_set) {
		std::vector<llvm::Type *> func_param_vec;
		std::list<TypeIdentifier *>::iterator tii;
		for (tii = fs->fp->fpl.begin(); tii != fs->fp->fpl.end(); tii++) {
			func_param_vec.push_back((*tii)->codeGen());
		}
		llvm::ArrayRef<llvm::Type *> func_param(func_param_vec);
		func_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), func_param, false);
		// if (fs->fr->is_set) {
		//	std::vector<llvm::Type *> func_return_vec;
		//	std::list<TypeIdentifier *>::iterator tii;
		//	for (tii = fs->fr->frl.begin(); tii != fs->fr->frl.end(); tii++) {
		//		func_return_vec.push_back((*tii)->codeGen());
		//	}
		//	llvm::ArrayRef<llvm::Type *> func_return(func_return_vec);
		//	func_type = llvm::FunctionType::get(func_return_vec, func_param, false);
		// } else {
		// }
	} else {
		func_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), false);
	}

	llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, fn, Module.get());

	BB = llvm::BasicBlock::Create(Context, "", func);
	Builder.SetInsertPoint(BB);

	if (b) {
		b->codeGen();
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
	for (si = s.begin(); si != s.end(); ++si) {
		if ((*si)->type == Statement::types::VAR_DECL) {
			(*si)->vds->codeGen();
		} else if ((*si)->type == Statement::types::COMPOSITE_TYPE_DEFN) {
			(*si)->ctds->codeGen();
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

}

void CompositeTypeDefn::codeGen() {
	if (type == CompositeTypeDefn::types::STRUCT) {
		s->codeGen(is_global);
	} else if (type == CompositeTypeDefn::types::UNION) {
		u->codeGen(is_global);
	} else if (type == CompositeTypeDefn::types::ENUM) {
		e->codeGen(is_global);
	}
}

void StructDefn::codeGen(bool is_global = false) {
	if (f) {
		if (f->is_set) {
			std::vector<llvm::Type *> struct_fields_vec;
			std::list<TypeIdentifier *>::iterator tii;
			for (tii = f->ti.begin(); tii != f->ti.end(); tii++) {
				struct_fields_vec.push_back((*tii)->codeGen());
			}
			llvm::ArrayRef<llvm::Type *> struct_fields(struct_fields_vec);
			llvm::StructType *struct_type = llvm::StructType::create(Module->getContext(), struct_fields, ident);

			if (is_global) {
				new llvm::GlobalVariable(*Module, struct_type, false, llvm::GlobalValue::ExternalLinkage, 0);
			} else {
				llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(struct_type, 0, ident, BB);
			}
		}
	} else {
		llvm::ArrayRef<llvm::Type *> struct_fields;
		llvm::StructType *struct_type = llvm::StructType::create(Module->getContext(), struct_fields, ident);

		if (is_global) {
			new llvm::GlobalVariable(*Module, struct_type, false, llvm::GlobalValue::ExternalLinkage, 0);
		} else {
			llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(struct_type, 0, ident, BB);
		}
	}
}

void UnionDefn::codeGen(bool is_global = false) {
	if (f) {
		if (f->is_set) {
			std::vector<llvm::Type *> union_fields_vec;
			std::list<TypeIdentifier *>::iterator tii;
			for (tii = f->ti.begin(); tii != f->ti.end(); tii++) {
				union_fields_vec.push_back((*tii)->codeGen());
			}
			llvm::ArrayRef<llvm::Type *> union_fields(union_fields_vec);
			llvm::StructType *union_type = llvm::StructType::create(Module->getContext(), union_fields, ident);

			if (is_global) {
				new llvm::GlobalVariable(*Module, union_type, false, llvm::GlobalValue::ExternalLinkage, 0);
			} else {
				llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(union_type, 0, ident, BB);
			}
		}
	} else {
		llvm::ArrayRef<llvm::Type *> union_fields;
		llvm::StructType *union_type = llvm::StructType::create(Module->getContext(), union_fields, ident);

		if (is_global) {
			new llvm::GlobalVariable(*Module, union_type, false, llvm::GlobalValue::ExternalLinkage, 0);
		} else {
			llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(union_type, 0, ident, BB);
		}
	}

}

void EnumDefn::codeGen(bool is_global = false) {

}

llvm::Type *TypeIdentifier::codeGen() {
	// REFER https://llvm.org/docs/LangRef.html#type-system
	llvm::Type *llvm_type;

	if (t->type_name->type_name == TypeName::type_names::BOOL) {
		llvm_type = llvm::Type::getInt8Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::CHAR) {
		llvm_type = llvm::Type::getInt8Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::BYTE) {
		llvm_type = llvm::Type::getInt8Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::INT) {
		llvm_type = llvm::Type::getInt64Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::INT8) {
		llvm_type = llvm::Type::getInt8Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::INT16) {
		llvm_type = llvm::Type::getInt16Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::INT32) {
		llvm_type = llvm::Type::getInt32Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::INT64) {
		llvm_type = llvm::Type::getInt64Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::UINT) {
		llvm_type = llvm::Type::getInt64Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::UINT8) {
		llvm_type = llvm::Type::getInt8Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::UINT16) {
		llvm_type = llvm::Type::getInt16Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::UINT32) {
		llvm_type = llvm::Type::getInt32Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::UINT64) {
		llvm_type = llvm::Type::getInt64Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::FLOAT32) {
		llvm_type = llvm::Type::getFloatTy(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::FLOAT64) {
		llvm_type = llvm::Type::getDoubleTy(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::FLOAT128) {
		llvm_type = llvm::Type::getFP128Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::STRING) {
		llvm_type = llvm::Type::getInt8Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::POINTER) {
		llvm_type = llvm::Type::getInt64PtrTy(Context, 0);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	} else if (t->type_name->type_name == TypeName::type_names::CUSTOM) {
		llvm_type = llvm::Type::getInt64Ty(Context);
	//	llvm::AllocaInst *llvm_alloca_inst = new llvm::AllocaInst(llvm_type, 0, ident, BB);
	}
	return llvm_type;

}