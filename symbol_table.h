#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <iostream>
#include <list>
#include <map>
#include "llvm/IR/Type.h"

class SymTable {
    public :
        static void Init();
        static void Destroy();
        static void EnterScope();
        static void InsertSymbol(std::string s_name, llvm::Type *s_type_name);
        static void InsertGlobalSymbol(std::string s_name, llvm::Type *s_type_name);
        static void ExitScope();
        static void Print();
};

#endif /* SYMBOL_TABLE_H */