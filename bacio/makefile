################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	grbindex.x
FOPTS=	
LOPTS=	
INCS=	
OBJS=	grbindex.o
LIBS=	/migr/data/wd21rg/nwprod/bacio.lib \
	/usr/ncep/lib/w3lib_32
################################################################################
$(CMD):	$(OBJS)
	f90 $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	makefile $(INCS)
grbindex.o:	grbindex.f
	f90 -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
