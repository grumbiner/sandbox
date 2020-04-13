BASE=../../../github.work/mmablib

INC=$(BASE)/include
LIB=$(BASE)/libombf_4.a

mv : mv.C
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
reading : reading.C
	g++ -Wall -O2 -DLINUX $< -I $(INC) $(LIB) -o $@
