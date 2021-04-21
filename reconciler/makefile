# Note that compiling with VERBOSE will provide substantial debugging output,
#   and VERBOSE2 will give an enormous amoung.
# Robert Grumbine 19 August 2003

#IBM
CC=xlC
CFLAGS=-DIBM -O2
#-DVERBOSE
#-DVERBOSE2
INCDIR=../shorelines -I../../include

#LINUX
CC=g++
CFLAGS=-Wall -DLINUX  -O2 -DFLIP
#-DVERBOSE
#-DVERBOSE2
INCDIR=shorelines -I/usr/local/include

#Common
TARGET=hycom
FAMILY=resops
#Common
TARGET=global_ice
FAMILY=llgrid
#Common
TARGET=gllamb
FAMILY=lambert
#Common
TARGET=$(TARG)
FAMILY=psgrid


# No changes below here ------------------------------------
all : toab refill refill.tries fill bathy paving

toab : toab.C resops2.h makefile
        $(CC) $(CFLAGS) toab.C -DTARGET=$(TARGET) -I$(INCDIR) -o toab

refill : refill.C geometry.C resops2.h makefile
        $(CC) $(CFLAGS) refill.C -DFAMILY=$(FAMILY) -DTARGET=$(TARGET) -I$(INCDIR) -o refill

refill.tries : refill.C geometry.C resops2.h makefile
        $(CC) -Wall -DLINUX  -O2 refill.C -DFAMILY=$(FAMILY) -DTARGET=$(TARGET) -I$(INCDIR) -o refill.tries

fill : fill.C geometry.C makefile
        $(CC) $(CFLAGS) fill.C -DFAMILY=$(FAMILY) -DTARGET=$(TARGET) -I$(INCDIR) -o fill

bathy : bathy.C
        $(CC) $(CFLAGS) bathy.C -DTARGET=$(TARGET) -I$(INCDIR) -o bathy

paving : paving.C geometry.C
        $(CC) $(CFLAGS) paving.C -DTARGET=$(TARGET) -DFAMILY=$(FAMILY) -o paving

delta : delta.C
        $(CC) $(CFLAGS) delta.C -DTARGET=$(TARGET) -I$(INCDIR) -o delta


