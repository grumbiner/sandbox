#/bin/ksh

FSRCS = \
        amsr2_1b_read_hdf5.f90 \
        amsr2_1b_write_hdf5.f90 \
        amsr2_1b_read_hdf5_interface.f90 \
        amsr2_1b_write_hdf5_interface.f90 \
        amsr2_1b_attr_read_hdf5.f90 \
        amsr2_1b_attr_read_hdf5_interface.f90 

F90OBJS = $(FSRCS:.f90=.o)

all:	$(F90OBJS) libhdf5_readwrite_f90.a

libhdf5_readwrite_f90.a:	$(F90OBJS)
		$(AR) $(AR_FLAGS) libhdf5_readwrite_f90.a $?

depend:
	makedepend $(FSRCS)

include ../config/rules.make
