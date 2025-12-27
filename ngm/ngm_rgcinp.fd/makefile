SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 11:21:07 AM on 10/28/94
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

SRCS=   CORNER.f    GETABC.f    INTERP.f    PRINTBIT.f  STATS.f     TASKF.f \
        CULL.f      GETBMT.f    LOLA.f      PRINTG.f    TASKA.f     TASKH.f \
        DIAG.f      GETHN.f     MAIN.f      RESTARTR.f  TASKB.f     TASKI.f \
        ERRSTOP.f   GLATS.f     NPOLE.f     RESTARTW.f  TASKC.f             \
        EXMOIS.f    GRDSET.f    WRTDAT.f    SG2SG.f     TASKD.f             \
        FIND.f      HORINT.f    POLY.f      PRINTIMS.f  SIGCON.f    TASKE.f \
        GRIBST.f    SNO16GET.f

OBJS=   CORNER.o    GETABC.o    INTERP.o    PRINTBIT.o  STATS.o     TASKF.o \
        CULL.o      GETBMT.o    LOLA.o      PRINTG.o    TASKA.o     TASKH.o \
        DIAG.o      GETHN.o     MAIN.o      RESTARTR.o  TASKB.o     TASKI.o \
        ERRSTOP.o   GLATS.o     NPOLE.o     RESTARTW.o  TASKC.o             \
        EXMOIS.o    GRDSET.o    WRTDAT.o    SG2SG.o     TASKD.o             \
        FIND.o      HORINT.o    POLY.o      PRINTIMS.o  SIGCON.o    TASKE.o \
        GRIBST.o    SNO16GET.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =            ncepxlf
LDFLAGS =	-bD:2040000000 -bmap:map -bmaxdata:2040000000 -bmaxstack:2040000000 -bnoquiet
LIBS =          -L/usr/local/lib -lessl -lmass -L/nwprod/lib -lw3_4 -lsp_4 -lbacio_4
CMD =           ngm_rgcinp
PROFLIB =       -lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED
FFLAGS =        -O -qarch=pwr3 -qintsize=4 -qrealsize=4 -qstrict -qddim -qmaxmem=-1 -qsource -qinitauto=FF -qflttrap=ov:zero:inv:en -qsigtrap -g \
		-qxref=full -qattr -qsave -qspillsize=548 -bloadmap:lm
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
