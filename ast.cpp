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

//extern llvm::LLVMContext context;

//void TopLevel::codeGen() {
	//std::cout << "I am the king !";
//}
llvm::Value* TopLevel::codeGen(llvm::LLVMContext& context) {
	return llvm::ConstantFP::get(context, llvm::APFloat(Val));
}

/*
class ImportDecl : public TopLevel {

};

class TypeDefn : public TopLevel {

};

class TypeFunc : public TopLevel {

};

class NSDefn : public TopLevel {

};

class FuncDefn : public TopLevel {

};

class FuncSign : public FuncDefn {

};

*/
