SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 10:55:50 AM on 02/29/96
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
SRCS=	DATA.f DELTA.f DIAG.f EXTRA.f INDXX.f INSST.f INTER.f \
	INTPL1.f INTR.f MARIAH.f NGMGRD.f NGMTRAJ.f NGMWND.f NTRNS.f \
	OBDEWP.f OBSFC.f OBTEMP.f OUTPT1.f OUTPT3.f OUTPUT.f OVERT.f \
	RADATA.f RDADP.f RDPE.f REDISK.f SEAMOD.f SFLAND.f SFSHIP.f \
	SMTR9.f SORT1.f TJANAL.f TJFCST.f TJPROJ.f TJREAD.f TJTRAJ.f \
	TRACE.f UPDATE.f WIND.f

OBJS=	DATA.o DELTA.o DIAG.o EXTRA.o INDXX.o INSST.o INTER.o \
	INTPL1.o INTR.o MARIAH.o NGMGRD.o NGMTRAJ.o NGMWND.o NTRNS.o \
	OBDEWP.o OBSFC.o OBTEMP.o OUTPT1.o OUTPT3.o OUTPUT.o OVERT.o \
	RADATA.o RDADP.o RDPE.o REDISK.o SEAMOD.o SFLAND.o SFSHIP.o \
	SMTR9.o SORT1.o TJANAL.o TJFCST.o TJPROJ.o TJREAD.o TJTRAJ.o \
	TRACE.o UPDATE.o WIND.o

# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
CF =		ncepxlf90
FC =            ncepxlf90
LDFLAGS =  -qsmp=noauto               
LIBS = -lessl -L/nwprod/lib -lw3_8 -ltdl_8 -lbacio_8 -lip_8 -lsp_8 -lbufr_8_64
CMD =		mdl_ngmtraj

FFLAGS =	-qintsize=8 -qrealsize=8   -O3 -qstrict -qfixed -qintlog

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

