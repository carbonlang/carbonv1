#include <iostream>
#include <list>
#include <map>
#include "llvm/IR/Type.h"
#include "symbol_table.h"

#define ALERT(str) std::cout << "\033[49;34m" << str << "\033[0m\n"

struct Symbol {
    std::string name;
    llvm::Type *type_name;
    Symbol(std::string tmp_name, llvm::Type *tmp_type_name) {
        name = tmp_name;
        type_name = tmp_type_name;
    }
};

typedef std::map<std::string, struct Symbol *> BlkSymbolMap;
std::list<BlkSymbolMap *> SymbolTable;
BlkSymbolMap GlobalSymbolTable;

void SymTable::Init() {
    ALERT("Init Symbol Table");
    /* Loop through each map in SymbolTable and clear it first */
    std::list<BlkSymbolMap *>::iterator it;
    for (it = SymbolTable.begin(); it != SymbolTable.end(); ++it) {
        (*it)->clear(); /* clear map */
        delete(*it);    /* delete map */
    }
    SymbolTable.clear();
    GlobalSymbolTable.clear();
}

void SymTable::Destroy() {
    ALERT("Destroy Symbol Table");
    /* Loop through each map in SymbolTable and clear it first */
    std::list<BlkSymbolMap *>::iterator it;
    for (it = SymbolTable.begin(); it != SymbolTable.end(); ++it) {
        (*it)->clear(); /* clear map */
        delete(*it);    /* delete map */
    }
    SymbolTable.clear();
    GlobalSymbolTable.clear();
}

void SymTable::EnterScope() {
    ALERT("Enter Scope");
    BlkSymbolMap *cur_bsm = new BlkSymbolMap();
    SymbolTable.push_back(cur_bsm);
}

void SymTable::InsertSymbol(std::string s_name, llvm::Type *s_type_name) {
    ALERT("Insert Symbol : " + s_name);
    BlkSymbolMap *cur_bsm;
    cur_bsm = SymbolTable.back();
    Symbol *s = new Symbol(s_name, s_type_name);
    cur_bsm->insert({s_name, s});
}

void SymTable::InsertGlobalSymbol(std::string s_name, llvm::Type *s_type_name) {
    Symbol *s = new Symbol(s_name, s_type_name);
    GlobalSymbolTable.insert({s_name, s});
}

void SymTable::ExitScope() {
    ALERT("Exit Scope");
    BlkSymbolMap *cur_bsm;
    cur_bsm = SymbolTable.back();
    /* Remove the last SymbolMap */
    cur_bsm->clear();
    delete(cur_bsm);
    SymbolTable.pop_back();
}

void SymTable::Print() {
    ALERT("*************************");
    ALERT("Print Entire Symbol Table");
    ALERT("*************************");
    ALERT("");

    ALERT("Global Symbol Table");
    ALERT("-----------------");
    /* Iterate map */
    for (auto i = GlobalSymbolTable.begin(); i != GlobalSymbolTable.end(); i++) {
        ALERT(i->first);
    }
    ALERT("");

    /* Loop through each map in SymbolTable and clear it first */
    std::list<BlkSymbolMap *>::iterator itr;
    int c = 1;
    for (itr = SymbolTable.begin(); itr != SymbolTable.end(); ++itr) {
        ALERT("Symbol Table : Level " << c);
        ALERT("---------------------");
        /* Iterate map */
        for (auto i = (*itr)->begin(); i != (*itr)->end(); i++) {
            ALERT(i->first);
        }
        ALERT("");
        c++;
    }
}
