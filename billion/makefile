
MODS = billion_kinds.mod triplets.mod things.mod 
OBJS = triplets.o things.o crystal.o

all: $(MODS) $(OBJS) main


%.o : %.F90
	gfortran -c -pg -std=f2003 -O2 $<
%.mod : %.F90
	gfortran -c -pg -std=f2003 -O2 $<

main : main.o $(OBJS)
	gfortran  $< $(OBJS) -o $(@)

clean :
	rm *.o *.mod
