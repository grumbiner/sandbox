# Create a Makefile for the program 'geology'

# geology depends on the 15 source files and two includes.
DYNAM = aabw.o advdif.o advstr.o upwell.o
CHEM = atmos.o biosrc.o bwsrc.o gasses.o ssink.o cco2.o
CONTR = oemain.o oestart.o oeout.o theory.o geolog.o
OBJS = $(CONTR) $(DYNAM) $(CHEM)

comp = /home/lang/f77 -c -u -cg89 -fast
link = /home/lang/f77 -fast

geology: $(OBJS)
	$(link) -o geology $(OBJS)

# Rules for the object files

oemain.o : oemain.f
	$(comp) oemain.f
aabw.o : aabw.f bw.inc
	$(comp) aabw.f
advdif.o : advdif.f
	$(comp) advdif.f
advstr.o : advstr.f
	$(comp) advstr.f
atmos.o : atmos.f chmptr.inc arrho.inc
	$(comp) atmos.f
biosrc.o : biosrc.f chmptr.inc
	$(comp) biosrc.f
bwsrc.o : bwsrc.f chmptr.inc arrho.inc bw.inc
	$(comp) bwsrc.f
cco2.o  : cco2.f
	$(comp) cco2.f
gasses.o : gasses.f chmptr.inc
	$(comp) gasses.f
geolog.o : geolog.f chmptr.inc arrho.inc
	$(comp) geolog.f
oestart.o : oestart.f chmptr.inc
	$(comp) oestart.f
oeout.o : oeout.f
	$(comp) oeout.f
ssink.o : ssink.f chmptr.inc
	$(comp) ssink.f
theory.o : theory.f
	$(comp) theory.f
upwell.o : upwell.f
	$(comp) upwell.f
chmptr.inc :

arrho.inc:


# Add a 'clean' rule:
clean:
	rm -f *.o
