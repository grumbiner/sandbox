# Note that compiling with VERBOSE will provide substantial debugging output,
#   and VERBOSE2 will give an enormous amoung.
# Robert Grumbine 19 August 2003

#IBM
CC=xlC
CFLAGS=-DIBM -O2 
INCDIR=shorelines -Immab.include
#LINUX
CC=g++
#CFLAGS=-Wall -DLINUX -O2 -DFLIP 
CFLAGS=-Wall -DLINUX -O2 -pg 
INCDIR=shorelines


#Common
TARGET=akgrid
FAMILY=psgrid

# No changes below here -------------------------------------------
all : refill bathy paving

refill : refill.C geometry.C makefile
	$(CC) $(CFLAGS) refill.C -DFAMILY=$(FAMILY) -DTARGET=$(TARGET) -I$(INCDIR) -o refill

bathy : bathy.C
	$(CC) $(CFLAGS) bathy.C -DTARGET=$(TARGET) -I$(INCDIR) -o bathy

paving : paving.C
	$(CC) $(CFLAGS) paving.C -DTARGET=$(TARGET) -DFAMILY=$(FAMILY) -I$(INCDIR) -o paving

