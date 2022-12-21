#
# Top level Makefile for the IASI code.
#

# 
# Determine the platform type.
#

OSFOUND = no
OSNAME = $(shell uname -s)


#
# Define compiler, compiler flag, linker, and linker flags.
#

#
# AIX platform.
#
ifeq ($(OSNAME),AIX)
    OSFOUND = yes
    FC = xlf
    FL = xlf
    LIBPATH=/usr/lib:/lib:/usr/local/lib
#    LINKINFO=-bnso -bI:/lib/syscalls.exp -bI:/lib/threads.exp
#   AIX does not know what the .f90 extension is so we set the following flag:
    COMMON_FLAGS=-qsuffix=f=f90 -g -bmaxdata:0x8000000000/dsa -bmaxstack:2500000000
    FC_FLAGS=-g -O3 -q64 -qmaxmem=-1 -bmaxdata:0x8000000000/dsa -bmaxstack:2500000000 -qrndsngl -qnomaf ${LINKINFO}
    FL_FLAGS= -bmaxdata:0x8000000000/dsa -bmaxstack:2500000000 -o
    FC90=xlf90
    FL90=xlf90
    FC90_FLAGS=-g -qnooptimize -qfullpath -O3 -q64 -qmaxmem=-1 -bmaxdata:0x800000000/dsa ${COMMON_FLAGS} ${LINKINFO}
    FC90_32_FLAGS=-O2 -qmaxmem=-1 -bmaxdata:0x80000000 ${COMMON_FLAGS}
    FL90_FLAGS=${COMMON_FLAGS} -o 
    CC=/usr/vacpp/bin/xlC
    CXX=/usr/vacpp/bin/xlC
    CC_FLAGS=-q64 -bmaxdata:0x80000000 -D_LARGE_FILES -DAIX -O3 -qmaxmem=-1 -qarch=auto ${LINKINFO}
    CXX_FLAGS=-q64 -bmaxdata:0x800000000/dsa -D_LARGE_FILES -DAIX -O3 -qmaxmem=-1 -qarch=auto ${LINKINFO}
    UTIL_TAG=aix
    INC=I
    NETCDFINC=/disk3/pub/letitias/netcdf4/include
    NETCDFLIB=/disk3/pub/letitias/netcdf4/lib
    HDF5INC=/disk3/pub/letitias/hdf5/include
    HDF5LIB=/disk3/pub/letitias/hdf5/lib
    BUFRINC=/disk3/pub/ysong/tool/BUFRLIB_v10-2-3
    BUFRLIB=/disk3/pub/ysong/tool/BUFRLIB_v10-2-3
    GRIB2INC=/disk3/pub/ysong/tool/g2lib-1.4.0
    GRIB2LIB=/disk3/pub/ysong/tool/g2lib-1.4.0
    BACIOLIB=/disk3/pub/ysong/tool/bacio-1.3
    BACIOINC=/disk3/pub/ysong/tool/bacio-1.3
    AR=ar
    AR_FLAGS=-ru -X64
endif

#
# Linux platform.
# 
ifeq ($(OSNAME),Linux)
    OSFOUND = yes
    FC=gfortran
    FL=gfortran
    COMMON_FLAGS=-O2 -static -m64 -DLINUX -g
    FC_FLAGS=${COMMON_FLAGS}
    FL_FLAGS=${COMMON_FLAGS} -o
    FC90=gfortran
    FL90=gfortran
    FC90_FLAGS=${COMMON_FLAGS} -ffree-line-length-200 
    FL90_FLAGS=${COMMON_FLAGS} -o
    CXX=g++
    CC=gcc
    CC_FLAGS=${COMMON_FLAGS}
    CXX_FLAGS=${COMMON_FLAGS}
    HDF5INC=/net/orbit247l/disk1/pub/ysong/gnu/tool/hdf5-1.8.8/include
    HDF5LIB=/net/orbit247l/disk1/pub/ysong/gnu/tool/hdf5-1.8.8/lib
    BUFRINC=/net/orbit247l/disk1/pub/ysong/gnu/tool/BUFRLIB_v10-0-1
    BUFRLIB=/net/orbit247l/disk1/pub/ysong/gnu/tool/BUFRLIB_v10-0-1
    AR=ar
    AR_FLAGS=-ru 
endif


#
# Export all variables for possible use in sub-makefiles.
#
export

#
# Define the INCLUDES and LIBS variables.
#
LOCALDIR = ..
INCLUDES = \
           -I$(LOCALDIR)/conversions \
           -I$(LOCALDIR)/modules \
           -I$(LOCALDIR)/bufr_readwrite \
           -I$(LOCALDIR)/hdf5_readwrite_f90 \
           -I$(LOCALDIR)/allocate \
           -I$(LOCALDIR)/time \
           -I$(LOCALDIR)/include \
           -I$(LOCALDIR)/hdf5_utils_f90 \
           -I$(LOCALDIR)/futils \
           -I$(BUFRINC) \
           -I$(HDF5INC)

LIBS = \
       -L$(LOCALDIR)/conversions -lconversions \
       -L$(LOCALDIR)/modules -lmodules \
       -L$(LOCALDIR)/bufr_readwrite -lbufr_readwrite \
       -L$(LOCALDIR)/hdf5_readwrite_f90 -lhdf5_readwrite_f90 \
       -L$(LOCALDIR)/allocate -lallocate \
       -L$(LOCALDIR)/include -linclude \
       -L$(LOCALDIR)/hdf5_utils_f90 -lhdf5_utils_f90 \
       -L$(BUFRLIB) -lbufr \
       -L$(LOCALDIR)/time -lwwwtime \
       -L$(LOCALDIR)/futils -lfutils \
       -L$(HDF5LIB) -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lz -lm  
#       -lifcore -limf 

#
# Default target to build everything.
#
all:	os_check subdirs \
        main_npr

#
# Check to make sure we are on a valid orperating system.
#
os_check:
	  @if [ $(OSFOUND) != yes ]; \
		then echo Unknown Operating System name: $(OSNAME); \
		exit 9; \
	 	fi

#
# Build all object files and put them into libraries.
#
subdirs:
	  @echo Operating System Name: $(OSNAME)
		@cd ../include; $(MAKE) -f Makefile.include
		@cd ../time; $(MAKE) -f Makefile.time
		@cd ../futils; $(MAKE) -f Makefile.futils
		@cd ../modules; $(MAKE) -f Makefile.modules
		@cd ../bufr_readwrite; $(MAKE) -f Makefile.bufr_readwrite
		@cd ../allocate; $(MAKE) -f Makefile.allocate
		@cd ../hdf5_utils_f90; $(MAKE) -f Makefile
		@cd ../hdf5_readwrite_f90; $(MAKE) -f Makefile
		@cd ../conversions; $(MAKE) -f Makefile.conversions

#
# Build the main programs.
#


main_npr:	main_npr.o
		rm -f $@
		$(FC90) $(FC90_FLAGS) main_npr.o $(INCLUDES) $(LIBS) -o $@
		rm $@.o

#
# Delete all the *.o, *.a, and *.mod files.
#
clean:
		rm -f *.o *.a *.mod
		cd ../modules; rm -f *.o *.a *.mod
		cd ../time; rm -f *.o *.a *.mod
		cd ../bufr_readwrite; rm -f *.o *.a *.mod
		cd ../include; rm -f *.o *.a *.mod
		cd ../futils; rm -f *.o *.a *.mod
		cd ../allocate; rm -f *.o *.a *.mod
		cd ../conversions; rm -f *.o *.a *.mod
		cd ../hdf5_utils_f90; rm -f *.o *.a *.mod
		cd ../hdf5_readwrite_f90; rm -f *.o *.a *.mod

#
# Include the suffixes file.
#

include ../config/rules.make

