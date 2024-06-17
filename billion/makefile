
MODS = billion_kinds.mod triplets.mod things.mod 
OBJS = triplets.o things.o crystal.o

all: $(MODS) $(OBJS) main

std = f95
#f95, f2003, f2008, f2018, f2023

#OPTS=-fopenmp
OPTS=

%.o : %.F90
	gfortran -c -Wall -std=$(std) -O2 $< $(OPTS)
%.mod : %.F90
	gfortran -c -Wall -std=$(std) -O2 $< $(OPTS)

main : main.o $(OBJS)
	gfortran  $< $(OBJS) -o $(@) $(OPTS)

clean :
	rm *.o *.mod
