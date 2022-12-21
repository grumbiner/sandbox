#Robert Grumbine 27 May 2016

#Linux
include ../makeall.mk

#-----------------------------------------------------

all: seaice_filtanal 

seaice_filtanal : filt3.o
	$(CPPLD) $(CPPLDFLAGS) filt3.o -o seaice_filtanal $(MMAB_LIBF4)

filt3.o : filt3.C 
	$(CPP) $(CPPOPTS) -c -I $(MMAB_INC) filt3.C 

