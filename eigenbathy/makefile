BASE=../../mmablib

INC=$(BASE)/include
LIB=$(BASE)/libombf_4.a

mv2 : mv2.C
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
mv : mv.C
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
reading : reading.C
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
