CXX=c++

all: carbon

OBJS+= ast.o parser.o lexer.o
LOCALBASE=/home/ps/git-repos/RE-flex
CFLAGS+= -Wall -Wno-deprecated-declarations -g -I${LOCALBASE}/include
LDFLAGS+= -L${LOCALBASE}/lib -lreflex
LLVMCONFIG=`llvm-config-13 --libs core native --cxxflags --ldflags` -fexceptions

carbon: ${OBJS}
	${CXX} -g -o $@ ${OBJS} ${LDFLAGS} ${LLVMCONFIG}

lexer.o: lexer.cc parser.cc
	${CXX} ${CFLAGS} ${LLVMCONFIG} -c -o $@ $<

lexer.cc: carbon.l
	reflex --header-file -o $@ $<

parser.o: parser.cc lexer.cc
	${CXX} ${CFLAGS} ${LLVMCONFIG} -c -o $@ $<

parser.cc: carbon.y
	bison -Wall -v -o $@ $<
	# bison -x $<
	# xsltproc /usr/share/bison/xslt/xml2xhtml.xsl carbon.xml > carbon.html

ast.o: ast.cpp
	${CXX} ${CFLAGS} ${LLVMCONFIG} -c -o $@ $<

clean:
	rm -f lexer.cc lexer.hh lex.yy.h
	rm -f parser.cc parser.hh location.hh position.hh stack.hh
	rm -f y.tab.c y.tab.h y.output lex.yy.cpp
	rm -f ast.o
	rm -f carbon *.o *~
	rm -f carbon.tab.c carbon.tab.h
	rm -f carbon.html carbon.xml parser.output

test:
	./carbon < ./testing/test1.crb

test2:
	./carbon < ./testing/test2.crb

compile:
	llc-13 output.ll -o=output.s
