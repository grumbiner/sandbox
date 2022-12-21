#Makefile for generating the executables for the sea ice concentration
#  product.
#Robert Grumbine 26 June 1997
  
include ../makeall.mk
 
#Linux
#VER=$(mmablib_ver)
#INCDIR=$(BASE)/mmab/$(VER)/include
#FC=ifort -O2 -c
#FLD=ifort
#CC=gcc -c -O2 -DLINUX
#LIBS = -L/nwprod/lib -lw3nco_4 -lbufr_4_64
LIBS = ${W3NCO_LIB4} ${BUFR_LIB4}

#Should be no changes below this point---------------------------------

seaice_ssmisubufr : 
	$(FC) $(FOPTS) ssmisu_decode.f
	$(CC) $(COPTS) ssmisu_binary.c -I $(MMAB_INC) 
	$(FLD) $(FLDFLAGS) ssmisu_decode.o ssmisu_binary.o $(LIBS) -o seaice_ssmisubufr
