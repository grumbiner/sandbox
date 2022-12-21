nbody : nbody.f90 point_mass.o makefile
	gfortran -O2 $< point_mass.o -o $(@)

%.o : %.f90
	gfortran -c -O2 $<
