SHELL=/bin/sh
#
SRCS=   ADABAT.f COF2GRD.f CONLEV.f EZSKCH.f \
        GESPRP.f IJ2LL.f ODABAT.f \
        TSTATS.f ZSTATS.f qfrmtp.f vexpnd.f \
        SHLLET.f vrtpsl.f xintrp.f

OBJS=   ADABAT.o COF2GRD.o CONLEV.o EZSKCH.o \
        GESPRP.o IJ2LL.o ODABAT.o \
        TSTATS.o ZSTATS.o qfrmtp.o vexpnd.o \
        SHLLET.o vrtpsl.o xintrp.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =	ncepxlf
LDFLAGS = -bD:2000000000 -bmap:map -bnoquiet

LIBS =          -L/usr/local/lib -lessl -lmass -L/nwprod/lib -lw3_4 -lsp_4 -qsmp=noauto

CMD =		ngm_gesprep
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS = -O -qarch=pwr3 -qstrict -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -q nosave -qspillsize=548 -bloadmap:lm

#FFLAGS =	 -F
#FFLAGS =	 -Wf"-ez"

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
	-rm -f $(OBJS) *.lst

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
