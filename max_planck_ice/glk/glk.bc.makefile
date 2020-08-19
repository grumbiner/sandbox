
LD=cc
#Reduction is the factor of reduction from the original 2.5 km grid.
#This number should be kept to integer divisors of 512.
REDUCTION=4

all : tsmap greduce ctof glk

tsmap : tsmapper.o
	$(LD) tsmapper.o -o tsmap

greduce : reducer.o freduce.o
	$(LD) reducer.o freduce.o -o greduce

ctof : cread_.o ctofmain.o
	$(LD) cread_.o ctofmain.o -lm -lf2c -o ctof

ctofmain.o : grid.inc

grid.inc :
	echo "      INTEGER nx, ny" > grid.inc
	echo "      PARAMETER (nx = 512 / "$(REDUCTION)")" >> grid.inc
	echo "      PARAMETER (ny = 512 / "$(REDUCTION)")" >> grid.inc

glk : tsmap greduce 
	echo $(REDUCTION) | ./greduce bathysyn.bin bathyout
	./tsmap bathyout tsshal tsdeep MASK bathy $(REDUCTION) 
	echo tsshal > fin; ./ctof < fin; mv fread tsshal.ftn
	echo tsdeep > fin; ./ctof < fin; mv fread tsdeep.ftn
	echo bathy > fin ; ./ctof < fin; mv fread bathy.ftn

clean :
	rm *.o 
	mv tsshal.ftn tsshal
	mv tsdeep.ftn tsdeep
	mv bathy.ftn bathy.glk
	rm fin bathy bathyout grid.inc tsmap greduce ctof 	
