all: things


%.o : %.F90
	gfortran -c -std=f95 -O2 $<
%.mod : %.F90
	gfortran -c -std=f95 -O2 $<

things : things.o triplets.o triplets.mod
	gfortran $< -o $(@)

