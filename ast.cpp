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
	//std::cout << "CG" << "\n";
	//std::list<Statement *>::iterator si;
	//for (si = s.begin(); si != s.end(); ++si){
	    //if ((*si)->type == Statement::types::VAR_DECL) {
			//	std::cout << "STMT"  << "\n";
				//(*si)->vds->codeGen();
			//}
	//}
}
