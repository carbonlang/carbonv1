CXX=c++

all: carbon

OBJS+= ast.o parser.o lexer.o
LOCALBASE= /usr/local
CFLAGS+= -Wall -I${LOCALBASE}/include
LDFLAGS+= -L${LOCALBASE}/lib -lreflex
LLVMCONFIG=`llvm-config-10 --libs core native --cxxflags --ldflags` -fexceptions -O0 -Wno-unused-but-set-variable

carbon: ${OBJS}
	${CXX} -o $@ ${OBJS} ${LDFLAGS} ${LLVMCONFIG}

lexer.o: lexer.cc parser.cc
	${CXX} ${CFLAGS} ${LLVMCONFIG} -c -o $@ $<

lexer.cc: carbon.l
	reflex --header-file -o $@ $<

parser.o: parser.cc lexer.cc
	${CXX} ${CFLAGS} ${LLVMCONFIG} -c -o $@ $<

parser.cc: carbon.y
	bison -o $@ $<

ast.o: ast.cpp
	${CXX} ${CFLAGS} ${LLVMCONFIG} -c -o $@ $<

clean:
	rm -f lexer.cc lexer.hh lex.yy.h
	rm -f parser.cc parser.hh location.hh position.hh stack.hh
	rm -f ast.o
	rm -f carbon *.o *~

test:
	./carbon < ./testprog/test1.crb
