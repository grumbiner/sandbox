MDIR = $(BASE)/icemodel/meteo
ODIR = $(BASE)/icemodel/mpi.source/misc
LIB  = $(PHOME)
OMBLIBF = /nwprod/omblib90/omblibf_4
OTHERS = $(OHOME)
LIBOBJS=$(LIB)/gau2l.o $(LIB)/w3ft01.o $(LIB)/w3fa09.o $(LIB)/gaulat.o $(LIB)/bsslz7.o $(LIB)/bsslz1.o

METEO = forfld.o thregt.o terp.o ffld.o overload.o 

all   : met wgrib 

wgrib : wgrib.o
	cc wgrib.o -lm -o wgrib

met   : incs $(METEO) getflx.o $(LIBOBJS)
	$(FLD) $(METEO) getflx.o $(W3) $(LIBOBJS) $(OMBLIBF) $(FLDFLAGS) -o met.$(pole)

met.old : incs $(METEO) getold.o
	$(FLD) $(METEO) getold.o $(OTHERS)/others.lib $(W3) $(FLDFLAGS) \
          -o met.old

incs    : icegrid.inc mgrid.inc physical.inc 

clean   :
	rm *.o *.inc 

clobber :
	rm *.o *.inc 

#include files
icegrid.inc : $(IHOME)/icegrid.$(pole)
	cp $(IHOME)/icegrid.$(pole) $(MDIR)/icegrid.inc
	chmod 644 $(MDIR)/icegrid.inc

mgrid.inc : $(IHOME)/mgrid.inc
	cp $(IHOME)/mgrid.inc $(MDIR)

physical.inc : $(IHOME)/physical.inc
	cp $(IHOME)/physical.inc $(MDIR)

# METEO routines
getflx.o : $(MDIR)/getflx.f mgrid.inc
	$(FFC) -c -O2 $(MDIR)/getflx.f
getold.o : $(MDIR)/getold.f mgrid.inc
	$(FFC) -c -O2 $(MDIR)/getold.f
forfld.o   : $(MDIR)/forfld.f icegrid.inc
	$(FFC) -c -O2 $(MDIR)/forfld.f
thregt.o  : $(MDIR)/thregt.f icegrid.inc mgrid.inc physical.inc
	$(FFC) -c -O2 $(MDIR)/thregt.f
terp.o  : $(MDIR)/terp.f icegrid.inc mgrid.inc
	$(FFC) -c -O2 $(MDIR)/terp.f

#Other routines
overload.o : $(ODIR)/overload.f
	$(FFC) -c -O2 $(ODIR)/overload.f 
