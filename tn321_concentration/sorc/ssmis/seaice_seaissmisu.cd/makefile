#Robert Grumbine 27 May 2016

include ../makeall.mk

#Linux
#CLD=g++
#INCDIR=$(BASE)/mmab/$(VER)/include
#LIBS=$(BASE)/mmab/$(VER)/libombc_4.a $(BASE)/mmab/$(VER)/libombf_4.a -lm
LIBS=$(MMAB_LIBC4) $(MMAB_LIBF4) -lm
 
#Should be no changes below this point---------------------------------
seaice_seaissmis : ssmis.main.o
	$(CPPLD) -DTEAM1 $< $(LIBS) $(CPPLDFLAGS) -o seaice_seaissmis

ssmis.main.o : ssmis.main.C
	$(CPP) $(CPPOPTS) -DTEAM1 $< 
