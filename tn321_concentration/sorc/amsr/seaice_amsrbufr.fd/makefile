SHELL=		/bin/sh
 
#IBM AIX values
LIBS =	-L/nwprod/lib -lw3_4 -lbacio_4 -lbufr_4_64
FFLAGS=	-Q -O3 -qsuffix=f=f -qxlf77=leadzero -bnoquiet
FC = xlf
CC = ncepxlc -I /nwprod/lib/sorc/omb/include 
CFLAGS =  -qarch=auto -q64 -O2 -DIBM -c

#LINUX values:
LIBS =	-L/nwprod/lib -lw3nco_4 -lbufr_4_64
FFLAGS= -O2 -c 
FC = ifort
FLD = ifort
CC = gcc -I $(BASE)/mmablib_v3.1.wcoss/include/
CFLAGS = -DLINUX -Wall -O2 -c -q64

#----------------------------------------------------------------

#SRCS=	amsr_decode.f amsre_binary.c 
#OBJS=   amsr_decode.o amsre_binary.o 
#CMD =  seaice_amsrbufr

#$(CMD):		$(OBJS)
seaice_amsrbufr:		
	$(FC) $(FFLAGS) amsr_decode.f
	$(CC) $(CFLAGS) amsre_binary.c
	$(FLD) amsr_decode.o amsre_binary.o -o $(@) $(LIBS)
	#$(FC) $(FFLAGS) -o  $(@) $(OBJS) $(LIBS)

amsre_binary.o : amsre_binary.c 
	$(CC) $(CFLAGS) amsre_binary.c
