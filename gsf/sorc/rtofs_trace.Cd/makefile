#IBM AIX
CC=ncepxlC
OPTS=-O2 -DIBM -I/nwprod/lib/sorc/omb/include
LIBS=-L/nwprod/lib/ -lombf_4

#LINUX
CC=g++
OPTS=-O2 -DLINUX -Wall -I$(BASE)/mmablib_v3.3.1/include
LIBS=$(BASE)/mmablib_v3.3.1/libombf_4.a

#----------- Should be no changes below here ---------------

all : rtofs_trace

rtofs_trace : rtofs_trace.C
	$(CC) $(OPTS) rtofs_trace.C -o rtofs_trace $(LIBS)
 
