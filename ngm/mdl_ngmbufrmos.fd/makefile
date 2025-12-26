SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 12:46:08 PM on 05/06/96
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
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
SRCS=	BCATGR.f BCLDMR.f BCMAP.f BDATE.f BDRIVR.f BGENER.f BNMLFQ.f \
	BPACK.f BPROB.f BPROJ.f BSPEC.f BSPECS.f BTMPDP.f BUFOPT.f \
	BUFRGN.f BUFRMOS.f BWDIR.f BWSPD.f GETCON.f LATLON.f RDWMO.f \
	SEACHK.f

OBJS=	BCATGR.o BCLDMR.o BCMAP.o BDATE.o BDRIVR.o BGENER.o BNMLFQ.o \
	BPACK.o BPROB.o BPROJ.o BSPEC.o BSPECS.o BTMPDP.o BUFOPT.o \
	BUFRGN.o BUFRMOS.o BWDIR.o BWSPD.o GETCON.o LATLON.o RDWMO.o \
	SEACHK.o

# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
CF =		ncepxlf90
FC =            ncepxlf90
LDFLAGS =	
LIBS = -L/nwprod/lib -lw3_8 -ltdl_8 -lbacio_8
CMD =		mdl_ngmbufrmos

FFLAGS =      -O3 -qstrict -qintlog -qfixed   -qrealsize=8 -qintsize=8	

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
