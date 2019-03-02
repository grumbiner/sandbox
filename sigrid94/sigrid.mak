CC=cc -c
LD=cc
FC=cf77 -c
FLD=segldr

BASE=dg.o process.o read_data.o read_k.o strpos.o trimnull.o
WEST=arcticw3.o writerecw.o
EAST=arctice3.o writerece.o
AA  = antarc3.o writerecaa.o

full : arctice arcticw antarct pgrida onea oneaa sibga

arctice : $(EAST) $(BASE) sigrid.h
	$(LD) $(EAST) $(BASE) -o arctice
test  : $(EAST) dg.o process.o read_data.o read_k.o strpos.o trimchar.o sigrid.h
	$(LD) $(EAST) dg.o process.o read_data.o read_k.o strpos.o trimchar.o -o arctice


arcticw : $(WEST) $(BASE) sigrid.h
	$(LD) $(WEST) $(BASE) -o arcticw

antarct : $(AA) $(BASE) sigridaa.h
	$(LD) $(AA) $(BASE) -o antarct

pgrida  : pgrida.o interval.o interp.o sigrid.h
	$(LD) pgrida.o interval.o interp.o -o pgrida

sibga   : sibg4.o mapll.o sigrid.h bgnh.h nesnh.h
	$(LD) sibg4.o mapll.o -o sibga

onea    : onea.o 
	$(FLD) onea.o -l/nwprod/w3lib -o onea

oneaa    : oneaa.o 
	$(FLD) oneaa.o -l/nwprod/w3lib -o oneaa

clean :
	rm *.o bgnh.h nesnh.h

bgnh.h : 
	cp /wd2/wd21/wd21rg/hibler/ic/bgnh.h .
	chmod 644 bgnh.h
 
nesnh.h : 
	cp /wd2/wd21/wd21rg/hibler/ic/nesnh.h .
	chmod 644 nesnh.h

#Domain specific code
antarc3.o    : antarc3.c
	$(CC) antarc3.c
arctice3.o   : arctice3.c
	$(CC) arctice3.c
arcticw3.o   : arcticw3.c
	$(CC) arcticw3.c
writerecw.o  : writerecw.c sigrid.h
	$(CC) writerecw.c
writerecaa.o : writerecaa.c sigridaa.h
	$(CC) writerecaa.c
writerece.o  : writerece.c sigrid.h
	$(CC) writerece.c
sibg4.o    : sibg4.o sigrid.h bgnh.h nesnh.h
	$(CC) sibg4.c

onea.o     : onea.f
	$(FC) onea.f
oneaa.o     : oneaa.f
	$(FC) oneaa.f


#Splice together the arctic ice.
pgrida.o   : pgrida.c
	$(CC) pgrida.c
interval.o : interval.c
	$(CC) interval.c
interp.o   : interp.c
	$(CC) interp.c

#Code shared between all domains
dg.o        : dg.c
	$(CC) dg.c
process.o   : process.c
	$(CC) process.c
read_data.o : read_data.c
	$(CC) read_data.c
read_k.o    : read_k.c
	$(CC) read_k.c
strpos.o    : strpos.c
	$(CC) strpos.c
trimnull.o  : trimnull.c
	$(CC) trimnull.c
trimchar.o  : trimchar.c
	$(CC) trimchar.c

#BG library code
mapll.o     : /wd2/wd21/wd21rg/library/mapll.c
	$(CC) /wd2/wd21/wd21rg/library/mapll.c
