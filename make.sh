#!/bin/bash

#rm lex.yy.cpp
#rm carbon
#reflex --bison carbon.l 
#c++ -Ireflex/include -o carbon lex.yy.cpp ~/reflex/lib/libreflex.a
#./carbon ./testprog/test1.crb

rm y.tab.c  y.tab.h  y.tab.o lex.yy.cpp y.output 
bison -y -d --report=state carbon.y
reflex  --flex --bison carbon.l
cc -Wall -Wunused -Wextra -c y.tab.c
c++ -Wall -Wunused -Wextra -o carbon y.tab.o lex.yy.cpp ~/reflex/lib/libreflex.a
./carbon < ./testprog/test1.crb

