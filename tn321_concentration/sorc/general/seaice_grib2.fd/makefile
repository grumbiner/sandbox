SHELL=/bin/sh
#
SRCS=	seaice_grib2.f
OBJS=	seaice_grib2.o
 
# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
#----- used with XLF on AIX  ---
#FC =            ncepxlf
#FFLAGS =        -O3 -qrealsize=4  -I $(INC)
#----- used with G95 on Linux ---
#FC =           g95
#FFLAGS =        $(INC)
#--------------------------------
#----- used with ifort on Linux ---
FC =           ifort
FFLAGS =        $(INC)
#--------------------------------

#LDFLAGS = -p -bnoquiet -bloadmap:mug
#LDFLAGS = -pg
LDFLAGS = 
INC = /nwprod/lib/incmod/g2_4 
LIBS = -L/nwprod/lib -lg2_4 -lw3nco_4 -lbacio_4\
       -L/usrx/local/64bit/lib -ljasper -lpng -lz

CMD =	seaice_grib2
PROFLIB =	-lprof


# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
