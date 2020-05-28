CXX=c++

all: carbon

OBJS+= parser.o lexer.o
LOCALBASE= /usr/local
CFLAGS+= -Wall -I${LOCALBASE}/include
LDFLAGS+= -L${LOCALBASE}/lib -lreflex

carbon: ${OBJS}
	${CXX} -o $@ ${OBJS} ${LDFLAGS}

lexer.o: lexer.cc parser.cc
	${CXX} ${CFLAGS} -c -o $@ $<

lexer.cc: carbon.l
	reflex --header-file -o $@ $<

parser.o: parser.cc lexer.cc
	${CXX} ${CFLAGS} -c -o $@ $<

parser.cc: carbon.y
	bison -o $@ $<

clean:
	rm -f lexer.cc lexer.hh lex.yy.h 
	rm -f parser.cc parser.hh location.hh position.hh stack.hh
	rm -f carbon *.o *~

test:
	./carbon < ./testprog/test1.crb
