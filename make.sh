#!/bin/bash

rm lex.yy.cpp
rm carbon
reflex carbon.l 
c++ -Ireflex/include -o carbon lex.yy.cpp ~/reflex/lib/libreflex.a
./carbon ./testprog/test1.crb
