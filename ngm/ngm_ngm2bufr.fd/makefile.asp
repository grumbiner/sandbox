SHELL=/bin/sh
#
SRCS=	bfrhdr.f bfrize.f guvtqp.f ngm2bufr.f

OBJS=	bfrhdr.o bfrize.o guvtqp.o ngm2bufr.o 

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		xlf
LDFLAGS = -bD:2000000000 -bmap:map  -bmaxstack:256000000  -bloadmap:lm
CMD =		ngm_ngm2bufr
PROFLIB =	-lprof


LIBS =          -L/usr/local/lib -lessl -lmass /nwprod/w3lib90/w3lib_4  \
     /nwprod/bufrlib90/bufrlib_4

FFLAGS = -O -qarch=auto -qrealsize=4 -qmaxmem=-1 -qsource -qsave -qxref=full -qattr -qspillsize=548  -g

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
