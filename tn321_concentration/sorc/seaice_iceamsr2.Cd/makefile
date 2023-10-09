#Robert Grumbine 27 May 2016

include ../makeall.mk
%.o: %.C
	$(CPP) $(CPPOPTS) $< -o $(@)


FFLAGS =	-O3 -convert big_endian -assume byterecl -real-size 64 -std90 -pg
CFLAGS = 	-O2 -DLINUX -std=c89 

OBJS = seaice_iceamsr2.o

seaice_iceamsr2 :  $(OBJS)
	$(CPPLD) $(CPPLDFLAGS) $(OBJS) -o $(@) $(MMAB_LIBF4) 

amsr2_l1b_scan : amsr2_l1b_scan.o
	$(CPPLD) $(CPPLDFLAGS) $< -o $(@) $(MMAB_LIBF4) 
