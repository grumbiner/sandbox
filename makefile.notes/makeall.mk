#Definitions to be shared by all makefiles
#Robert Grumbine 8 March 2016
SHELL=/bin/sh

#Demand that these be set in environment:
#BASE=$(BASE)
#VER=$(MMAB_VER)
MMAB_INC=-I $(BASE)/$(VER)/include/
MMAB_LN=$(BASE)/$(VER)/include/
MMAB_LIB=-L $(BASE)/$(VER)/
MMAB_SRC=$(BASE)/$(VER)/sorc/

NETCDF_INCLUDE=-I /Users/rmg3/usrlocal/include/
NETCDF_LD_FLAGS_C=-L /Users/rmg3/usrlocal/lib/

W3NCO_LIB4=/Volumes/ncep/nceplibs/libw3nco_4.a
W3EMC_LIB4=/Volumes/ncep/nceplibs/libw3emc_4.a
BUFR_LIB4=/Volumes/ncep/nceplibs/libbufr1_4.a

# ---------------------------------------------------------------------
#All:
NCEPLIB=$(BUFR_LIB4) $(W3EMC_LIB4) $(W3NCO_LIB4)
LIBS=$(MMAB_LIB) -lombc_4 -lombf_4 -lm

#some wcoss: -limf -lirc

#------------------- should need no changes below here ------------------------
#Compilers and their options
FC=ifort
FOPTS=-c -std95 -O2 $(MMAB_INC)
#FOPTS=-c -O2 $(MMAB_INC) $(NETCDF_INCLUDE)

##Home desk:
#FC=gfortran
###FOPTS=-c -std=f95 -O2 -I$(MMAB_INC) $(NETCDF_INCLUDE)

FLD=$(FC)
FLDFLAGS=

CPP=g++
#CPPOPTS= -c -ansi -Wall -O2 -DLINUX -DCPLUS -I$(MMAB_INC) $(NETCDF_INCLUDE)
CPPOPTS= -c -ansi -Wall -O2 -DLINUX -DCPLUS $(MMAB_INC) $(NETCDF_INCLUDE)
CPPLD=g++
CPPLDFLAGS=-lombf_4 -lombc_4

CC=gcc
COPTS=-c -ansi -O2 -DLINUX $(MMAB_INC) $(NETCDF_INCLUDE)

#Building elements
%.o: %.C
	$(CPP) $(CPPOPTS) $< -o $(@)
%.o: %.c
	$(CC) $(COPTS) $< -o $(@)

%.o: %.f
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F90
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.f90
	$(FC) $(FOPTS) $< -o $(@)


#--------------------------------------------------------------
#Another take:
FCMDS = dcheck.ross ross.data.check harmfront hcust hiter irfront iriter sigdri fanalyze t2front gauss.random tmodulate superm dcheck.deformat testing

CCMDS = harmonic1 harmonicpost harmonicquarter splice30

all : $(FCMDS) $(CCMDS)

FOBJS = demod.o detrnd.o harmim.o harmin.o readin.o signif.o tidefr.o sssum.o sumcos.o sort.o p.o pythag.o rin.o ritout.o velcmp.o yes.o filt.o ftrc.o harmrm.o harmrn.o 

#--------- Relatively unchanged below here --------------
.o: %.f
        gfortran -c $<

COPTS = -I ~/usrlocal/mmablib/include ~/usrlocal/mmablib/libombf_4.a -DLINUX -DGRIDTYPE=global_ice
.o: %.C
        g++ -c $(COPTS) $<

$(FCMDS) : %: %.f $(FOBJS)
        gfortran $< $(FOBJS) ../lib/linpack.a ../lib/librefblas.a ../lib/imsl.a -o $(@)

$(CCMDS) : %: %.C
        g++ $< $(COPTS) -o $(@)

clean :
        rm $(FOBJS)

distclean :
        rm $(CCMDS) $(FCMDS)

