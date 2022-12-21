#Robert Grumbine 27 May 2016

include ../makeall.mk

##linux 
#CLD=g++
#RPP=g++ -c -Wall -O2 -I$(INCDIR) -DHIRES -DTEAM2 -DLINUX -DCPLUS
#VER=$(mmablib_ver)
#INCDIR=$(BASE)/mmab/$(VER)/include
LIBS=$(MMAB_LIBC4) $(MMAB_LIBF4) -lm

#Should be no changes below this point---------------------------------

seaice_seaissmi : ssmi.main.o
	$(CPPLD) $(CPPLDFLAGS) ssmi.main.o $(LIBS) -o seaice_seaissmi

ssmi.main.o : ssmi.main.C
	$(CPP) $(CPPOPTS) ssmi.main.C 
