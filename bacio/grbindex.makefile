################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	grbindex.xc
FOPTS=	
LOPTS=	
INCS=	
OBJS=	grbindex.o \
	grbindex.md.o
LIBS=	bacio.a /nwprod/w3lib90/w3lib
################################################################################
$(CMD):	$(OBJS) bacio.a
	segldr $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	makefile $(INCS)
grbindex.o:	grbindex.f
	f90 -c $(FOPTS) $*.f
grbindex.md.o:	grbindex.md.f
	f90 -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
