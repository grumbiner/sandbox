#Makefile for generating the executables for the sea ice concentration
#  product.
#Robert Grumbine 26 June 1997

include ../makeall.mk

#Linux
#VER=$(mmablib_ver)
#INCDIR=$(BASE)/mmab/$(VER)/include
#FC=ifort -O2 -c
#FLD=ifort
#CC=gcc 
#COPTS=-c -O2 -q64 -DLINUX
LIBS = $(BUFR_LIBd)

#Should be no changes below this point---------------------------------

seaice_ssmibufr : 
	$(FC) $(FOPTS) ssmi.bufr.f
	$(CC) $(COPTS) binary.c -I$(MMAB_INC)
	$(FLD) ssmi.bufr.o binary.o $(LIBS) -o seaice_ssmibufr
