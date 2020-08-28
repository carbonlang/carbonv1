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
llvm::Value* TopLevel::codeGen(llvm::LLVMContext& context, llvm::IRBuilder<>& builder, std::unique_ptr<llvm::Module>& module) {
	llvm::Value *L = llvm::ConstantFP::get(context, llvm::APFloat(10.0));
	llvm::Value *R = llvm::ConstantFP::get(context, llvm::APFloat(20.0));
	return builder.CreateFAdd(L, R, "addtmp");
}
