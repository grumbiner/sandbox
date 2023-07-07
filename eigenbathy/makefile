BASE=$(HOME)/usrlocal/mmablib

INC=$(BASE)/include
LIB=$(BASE)/libombf_4.a

%.o: %.C
	g++ -c -Wall -I $(INC) $< -o $(@)

mv2 : mv2.C shared.o
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
mv : mv.C shared.o
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
reading : reading.C shared.o
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
tmp : tmp.C shared.o
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@

all : mv2 mv reading tmp
