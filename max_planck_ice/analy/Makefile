#Makefile for the analysis (graphics) programs
#These are mostly NCAR Graphics based (those programs which
#  refer to NCARLD in the build process)
#Robert Grumbine
#Last Modified 11 Jan 1996
#xpm files added 20 February 1997

notncar : mag corners image northxpm southxpm

analysis : map2 mag corners image mapcont northxpm southxpm

corners : corners.o mapxy.o icegrid.inc
	$(LD) corners.o mapxy.o $(FLDFLAGS) -o corners

mag     : magnify.o icegrid.inc
	$(LD) magnify.o $(FLDFLAGS) -o mag

mapcont : mapcont.o FX.o FY.o $(PHOME)/polarflib icegrid.inc
	$(NCARLD) mapcont.o FX.o FY.o $(PHOME)/polarflib $(FLDFLAGS) -o mapcont

map2	: map2.o 
	$(NCARLD) map2.o -o map2

image   : mapim.o imprep.o icegrid.inc
	$(LD) mapim.o imprep.o $(FLDFLAGS) -o image
 
clean :
	rm *.o

clobber : clean
	rm *.inc

icegrid.inc : ../mpi.source/icegrid.$(pole)
	cp ../mpi.source/icegrid.$(pole) icegrid.inc

corners.o : corners.f icegrid.inc
	$(FC) corners.f
magnify.o : magnify.f icegrid.inc
	$(FC) magnify.f
mapcont.o : mapcont.f icegrid.inc
	$(FC) mapcont.f
map2.o : map2.f icegrid.inc
	$(FC) map2.f
mapim.o : mapim.f icegrid.inc
	$(FC) mapim.f

FX.o    : FX.f icegrid.inc
	$(FC) FX.f
FY.o    : FY.f icegrid.inc
	$(FC) FY.f
mapxy.o : $(PHOME)/mapxy.f
	$(FC) $(PHOME)/mapxy.f
