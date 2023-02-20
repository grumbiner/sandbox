# Note that compiling with VERBOSE will provide substantial debugging output,
#   and VERBOSE2 will give an enormous amoung.
# Robert Grumbine 19 August 2003

#IBM
CC=xlC
CFLAGS=-DIBM -O2 
#-pg 
#-DVERBOSE 
#-DVERBOSE2 
INCDIR=shorelines -Immab.include

#LINUX
CC=g++
CFLAGS=-Wall -DLINUX -O2 -DFLIP 
#-pg 
#-DVERBOSE -DVERBOSE2 
INCDIR=shorelines
LIBDIR=/usr/local/lib/libcpp?

#Common
TARGET=$(TARG)
FAMILY=psgrid


# No changes below here -------------------------------------------
all : refill2 bathy 

refill2 : polar.C geometry.C makefile
	$(CC) $(CFLAGS) polar.C -DFAMILY=$(FAMILY) -DTARGET=$(TARGET) -Dpsgrids -I$(INCDIR) -o refill2 $(LIBDIR)

bathy : bathy.C
	$(CC) $(CFLAGS) bathy.C -DTARGET=$(TARGET) -I$(INCDIR) $(LIBDIR) -o bathy

