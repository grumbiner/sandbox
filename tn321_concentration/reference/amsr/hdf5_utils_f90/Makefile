#/bin/ksh

FSRCS = hdf5_interface.f90 \
        hdf5_attribute_interface.f90

F90OBJS = $(FSRCS:.f90=.o)

all:    $(F90OBJS) libhdf5_f90.a

libhdf5_f90.a:    $(F90OBJS)
		$(AR) $(AR_FLAGS) libhdf5_utils_f90.a $?

include ../config/rules.make
