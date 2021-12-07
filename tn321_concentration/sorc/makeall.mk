#Definitions to be shared by all makefiles
#Robert Grumbine 8 March 2016
SHELL=/bin/sh

#Demand that these be set in environment:
#BASE=$(BASE)
VER=$(MMAB_VER)

MMAB_INC=$(BASE)/mmab/$(VER)/include/
MMAB_LIB=-L $(BASE)/mmab/$(VER)/
MMAB_SRC=$(BASE)/mmab/$(VER)/sorc/

#------------------- should need no changes below here ------------------------
#Compilers and their options

FC=ifort
#FOPTS=-c -std90 -O2 -I$(MMAB_INC)
FOPTS=-c -O2 -I$(MMAB_INC)
FLD=ifort
FLDFLAGS=

CPP=g++
#CPPOPTS= -c -ansi -Wall -O2 -DLINUX -DCPLUS -I$(MMAB_INC)
CPPOPTS= -c -Wall -O2 -DLINUX -DCPLUS -I$(MMAB_INC)
CPPLD=g++
CPPLDFLAGS=-lombf_4 -lombc_4

CC=gcc
COPTS=-c -Wall -O2 -DLINUX -I$(MMAB_INC)

#Building elements
%.o: %.C
	$(CPP) $(CPPOPTS) $< -o $(@)
%.o: %.c
	$(CC) $(COPTS) $< -o $(@)

%.o: %.f
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F90
	$(FC) $(FOPTS) $< -o $(@)

