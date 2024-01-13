#Makefile for initial condition programs
#Robert Grumbine
#Last Modified 20 February 1996

#Should need no changed below here ------------------------

#This should rely on a passed-in pole
all : restart.north restart.south restart.ak restart.glk tomodel.north tomodel.south

tomodel.south : $(LIBDIR) tomodel.C
	$(GPP) -O2 -Wall tomodel.C -D$(D) $(LIBDIR) -I$(INCDIR) -DGRIDTYPE=southgrid -o tomodel.south
 
tomodel.north : $(LIBDIR) tomodel.C
	$(GPP) -O2 -Wall tomodel.C -D$(D) $(LIBDIR) -I$(INCDIR) -DGRIDTYPE=northgrid -o tomodel.north
 
restart.ak : icegrid.ak restart
	mv restart restart.ak

restart.north : icegrid.north restart
	mv restart restart.north

restart.south : icegrid.south restart
	mv restart restart.south

restart.glk : clean icegrid.glk restart
	mv restart restart.glk
	make clean

clean : 
	rm *.o icegrid.inc

icegrid.ak : $(IHOME)/icegrid.ak
	cp $(IHOME)/icegrid.ak .
	if ( test -f icegrid.inc )  then  ( chmod 644 icegrid.inc ) fi
	mv icegrid.ak icegrid.inc
	chmod 644 icegrid.inc
 
icegrid.north : $(IHOME)/icegrid.north
	cp $(IHOME)/icegrid.north .
	if ( test -f icegrid.inc )  then  ( chmod 644 icegrid.inc ) fi
	mv icegrid.north icegrid.inc
	chmod 644 icegrid.inc
 
icegrid.south : $(IHOME)/icegrid.south
	cp $(IHOME)/icegrid.south .
	if ( test -f icegrid.inc )  then  ( chmod 644 icegrid.inc ) fi
	mv icegrid.south icegrid.inc
	chmod 644 icegrid.inc

icegrid.glk : $(IHOME)/icegrid.glk
	cp $(IHOME)/icegrid.glk .
	if ( test -f icegrid.inc )  then  ( chmod 644 icegrid.inc ) fi
	mv icegrid.glk icegrid.inc
	chmod 644 icegrid.inc
 
restart : restart.o filtsst.o filtsalt.o updatec.o filtconc.o sanity.o tfreez.o
	$(LD) restart.o $(LIBDIR) filtsst.o filtsalt.o updatec.o filtconc.o  sanity.o tfreez.o $(LIBDIR) $(FLDFLG) -o restart

restart.o : icegrid.inc restart.f 
	$(FC) restart.f -I$(INCDIR)

tfreez.o  : $(IHOME)/misc/tfreez.f
	$(FC) $(IHOME)/misc/tfreez.f

toxpm.south : 
        $(GPP) -O2 toxpm.C -DGRIDTYPE=southgrid -D$(D) $(LIBDIR) -I$(INCDIR) -o toxpm.south
 
toxpm.north : 
        $(GPP) -O2 toxpm.C -DGRIDTYPE=northgrid -D$(D) $(LIBDIR) -I$(INCDIR) -o toxpm.north
 
toxpm.ak :
        $(GPP) -O2 toxpm.C -DGRIDTYPE=akgrid -D$(D) $(LIBDIR) -I$(INCDIR) -o toxpm.ak

