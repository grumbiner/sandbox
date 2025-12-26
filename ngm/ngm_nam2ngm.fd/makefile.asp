SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 10:56:31 AM on 11/10/98
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=	DIAG.f ERRSTOP.f ETA2NGM.f GETE104.f GRDSET.f PRINTG.f VEXPND.f CORNER.f ADABAT.f GENLL.f \
	XINTRP.f RESTARTR.f RESTARTW.f STATS.f VTERP.f WRTDAT.f QFRMTP.f PRINTNGM.f VWEIGHTS.f

OBJS=	DIAG.o ERRSTOP.o ETA2NGM.o GETE104.o GRDSET.o PRINTG.o VEXPND.o CORNER.o ADABAT.o GENLL.o \
	XINTRP.o RESTARTR.o RESTARTW.o STATS.o VTERP.o WRTDAT.o QFRMTP.o PRINTNGM.o VWEIGHTS.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		xlf
LDFLAGS = -bD:2040000000 -bmap:map -bmaxdata:2040000000 -bmaxstack:2040000000 -bnoquiet
LIBS =          -L/usr/local/lib -lessl -lmass /nwprod/w3lib90/w3lib_4_604 \
                  /nwprod/w3lib90/bacio_4_604
CMD =		ngm_eta2ngm
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =  -O -qarch=604 -qstrict -qddim -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -qnosave -qspillsize=548 -bloadmap:lm
#FFLAGS =         -Ovector3	
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
