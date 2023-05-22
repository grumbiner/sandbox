#Makefile For creating boundary conditions for the sea ice model
#Robert Grumbine
#Last Modified 21 December 1995

OTHDIR = $(OHOME)
SDIR = $(RHOME)
OBJS= $(LIBDIR)/mapxy.o $(LIBDIR)/w3ft01.o $(LIBDIR)/levitus.o $(LIBDIR)/arcdis.o 

bc     : masker listpts ocean bathy

masker : incs terp.o terph.o masker.o mclean.o 
	$(FLD) terp.o terph.o masker.o mclean.o $(OBJS) $(FLDFLAGS) -o masker

listpts : icegrid.inc listpts.o 
	$(FLD) listpts.o $(OBJS) $(FLDFLAGS) -o listpts

ocean : incs tsmaker.o layers.o adj6.o regrid.o tsexam.o tfreez.o
	$(FLD) tsmaker.o layers.o adj6.o regrid.o \
              tsexam.o tfreez.o $(OBJS) $(FLDFLAGS) -o ocean

bathy : icegrid.inc mgrid.inc bathy.o terph.o
	$(FLD) bathy.o terph.o $(OBJS) -o bathy

incs    : icegrid.inc mgrid.inc physical.inc oml.inc 

clean   :
	rm *.o *.inc ../meteo/*.inc 

clobber : 
	rm *.o *.inc ../meteo/*.inc ocean listpts masker \
           maskout maskrun maskin bathy bathy.bin

#include files
icegrid.inc :
	cp $(SDIR)/mpi.source/icegrid.inc $(SDIR)/bc
	cp $(SDIR)/mpi.source/icegrid.inc $(SDIR)/meteo
mgrid.inc :
	cp $(SDIR)/mpi.source/mgrid.inc $(SDIR)/bc
	cp $(SDIR)/mpi.source/mgrid.inc $(SDIR)/meteo
physical.inc :
	cp $(SDIR)/mpi.source/physical.inc $(SDIR)/bc
oml.inc :
	cp $(SDIR)/mpi.source/oml.inc $(SDIR)/bc

# Masking routines
masker.o  : $(SDIR)/bc/masker.f icegrid.inc 
	$(FFC) -c $(SDIR)/bc/masker.f
mclean.o  : $(SDIR)/bc/mclean.f 
	$(FFC) -c $(SDIR)/bc/mclean.f
terp.o  : $(SDIR)/meteo/terp.f icegrid.inc mgrid.inc
	$(FFC) -c $(SDIR)/meteo/terp.f

listpts.o  : $(SDIR)/bc/listpts.f icegrid.inc
	$(FFC) -c $(SDIR)/bc/listpts.f

# Ocean gridding routines
tsmaker.o : $(SDIR)/bc/tsmaker.f icegrid.inc
	$(FFC) -c $(SDIR)/bc/tsmaker.f
layers.o  : $(SDIR)/bc/layers.f
	$(FFC) -c $(SDIR)/bc/layers.f
adj6.o : $(SDIR)/bc/adj6.f icegrid.inc physical.inc
	$(FFC) -c $(SDIR)/bc/adj6.f
regrid.o : $(SDIR)/bc/regrid.f icegrid.inc 
	$(FFC) -c $(SDIR)/bc/regrid.f
tsexam.o : $(SDIR)/bc/tsexam.f icegrid.inc physical.inc oml.inc
	$(FFC) -c $(SDIR)/bc/tsexam.f
#terph.o  : $(SDIR)/bc/terph.f icegrid.inc mgrid.inc
#	$(FFC) -c $(SDIR)/bc/terph.f

#Library routines
tfreez.o     : $(SDIR)/mpi.source/misc/tfreez.f
	$(FFC) -c $(SDIR)/mpi.source/misc/tfreez.f
