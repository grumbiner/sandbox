CMD = model_os

#Generic rule for compilation:
.C.o :
	g++ -Wall -O2 -DLINUX -c $< -I ~/usrlocal/mmablib/include

#Generic rule for linking to executable
$(CMD) : $(CMD).o
	g++ -Wall -O2 -DLINUX $(@).o -o $(@)

