#IBM AIX
CLD=ncepxlC 
RPP=ncepxlC -c -O2 -I$(INCDIR) -DTEAM2 -DHIRES -DIBM -DCPLUS 
INCDIR=/nwprod/lib/sorc/omb/include 
LIBS = -L/nwprod/lib/ -lombc_4 -lombf_4 -L /nwprod/lib -lw3_4 -lm

#linux 
CLD=g++ -O2 -Wall -DTEAM2 -DLINUX -DCPLUS
VER=$(mmablib_ver)
INCDIR=$(BASE)/mmablib.$(VER)/include
RPP=g++ -c -Wall -O2 -I$(INCDIR) -DTEAM2 -DLINUX -DCPLUS
LIBS=$(BASE)/mmablib.$(VER)/libombc_4.a $(BASE)/mmablib.$(VER)/libombf_4.a -lm

#Should be no changes below this point---------------------------------

seaice_blend : blend.o
	$(CLD) blend.o $(LIBS) -o  seaice_blend

blend.o : blend.C 
	$(RPP) blend.C 

