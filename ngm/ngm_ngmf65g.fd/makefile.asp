################################################################################
# 
#     Makefile for Nested Grid Model
#
#     Use:
#     make         -  build the executable
#     make clean   -  start with a clean slate
#
#     The following macros will be of interest:
#
#         TARGET   - name of the executable
#         FC       - name of Fortran compiler
#         CPP      - name of CPP
#         ARCH     - architecture
#         CPPFLAGS - CPP flags
#         OPTS     - compiler code optimizations
#         LIST     - source listing
#         SMP      - threading
#         TRAPS    - runtime traps for floating point exceptions
#         PROFILE  - source code profiling ( -pg )
#         DEBUG    - -g
#         MEM      - user data area and stack size
#         MAP      - load map
#         W3LIB    - w3lib
#         BACIO    - bacio lib
#         ESSL     - ESSL library
#         MASS     - MASS library
#         SEARCH   - library search location
#
#     History:
#
#         Original version: J. Tuccillo, 30 August 1999
#
#################################################################################
#
# Define the name of the executable
#
TARGET = ngm_ngmfcst
#
# CPP, Compiler, and Linker Options
#
FC       = xlf_r
CPP      = /lib/cpp -P
ARCH     = pwr3
CPPFLAGS = 
OPTS     = -O3 -qstrict -qarch=$(ARCH) -qmaxmem=-1 -NS2000
LIST     = -qsource -qxref=full -qattr=full -qsave
SMP      = -qsmp=noauto
TRAPS    = 
PROFILE  =
DEBUG    = 
MEM      = -bmaxdata:1000000000 -bmaxstack:256000000
MAP      = -bmap:map -bloadmap:lm
W3LIB    = /nwprod/w3lib90/w3lib_4_$(ARCH)
BACIO    = /nwprod/w3lib90/bacio_4_$(ARCH)
ESSL     = -lessl_r
MASS     = -lmass_r -lmassv
#
# Assemble Options
#
FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG)
FFLAGST  = $(OPTS) $(LIST) $(SMP) $(TRAPS) $(PROFILE) $(DEBUG)
LDFLAGS  = $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS     = $(W3LIB) $(BACIO) $(ESSL) $(MASS)
#
#
# Non-threaded object files
#
OBJS=	ALLGRIDS.o  OUTAREA.o   OUTSCALE.o  SHEMPRE.o  LAB2PDS.o  \
	BIGCHG.o    HMDY.o      OUTCLOSE.o  OUTSIG2P.o  SKSKINT0.o \
	BITHOLE.o   HQCOEF.o    OUTDAT2P.o  OUTSIGMA.o  SKSTEP1.o \
	BITOUT.o    JULDAY.o    OUTFDTUV.o  OUTSMOO.o   SKSTEPAL.o \
	BLEND.o     LATBND.o    OUTFIXED.o  OUTSVALU.o  SKTGRND0.o \
	CHARNOCK.o  LL2GR.o     OUTGRWD.o   P2KAP.o     SMOOTH.o    SMOOTHA.o \
	COSZEN.o    LWCLDSLW.o  OUTHORIZ.o  PARITY.o    RDHSCATR.o  \
	DEWPOINT.o  OUTHYDRO.o  PHYPREP.o   RDHGATHR.o  SOUNDING.o \
	DIAGFCST.o  LWFLUX.o    OUTLIFT.o   PHYSICS.o   STATS.o     FEQUEUE.o \
	DIAGPRT.o   LWRADIAT.o  OUTLOLA.o   PRECIP.o    SWCLEAR.o \
	DRYADB.o    MAIN.o      OUTMCONV.o  PRINTBIT.o  SWCLOUDY.o \
	DUMPBIT.o   MIXDIAG.o   OUTNULAB.o  PRINTTIM.o  RDSTEP1.o   SWRADIAT.o \
	MSTADB.o    OUTOMEGA.o  PRT.o       RDSTEPAL.o  SWSET.o     SKSBSOIL.o\
	O3CLIMO.o   OUTOPEN.o   PRT25.o     RESTARTR.o  TEMPADJ.o   \
	DUMPINT.o   O3INT.o     OUTPACK.o   PRTSQARE.o  RESTARTW.o  TOPRT25.o \
	DUMPREAL.o  OUTPHYS.o   SEALDIAG.o  VAPTAB.o    OUTOPENGB.o GRIBIT.o\
	ERRPRINT.o  ONEWAY.o    OUTPREP.o   RDCLDGEN.o  SFCEXCH.o   ZONEDIAG.o \
	OUT2FILE.o  OUTPUT_new.o  SFCMIX.o    CONVRT.o  GRIBITN.o    GTBITS.o \
	FILLHOLE.o  OUTABVOR.o  OUTRPAUS.o  SHEM.o      I84TGB.o    IDSDEF.o \
	ISHELL.o ASNUNIT.o 

#
# Threaded object files
#
OBJST=	LWCRUNCH.o ONEGRID.o
#
# Includes
#
INCLUDES= parmodel
#
# Common Blocks
#
COMMS=

DEPS= $(COMMS) $(INCLUDES)

.SUFFIXES:	.F .f .o

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET):	$(OBJS) $(OBJST)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(OBJST) $(LIBS)

$(OBJS):	$(DEPS)
	$(FC) $(FFLAGS) -c $<

$(OBJST):	$(DEPS)
	$(FC) $(FFLAGST) -c $<

clean:	
	/bin/rm -f  *.lst *.o
#
