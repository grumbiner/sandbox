shallow : main.F90 types.o
	gfortran -O2 -std=f95 main.F90 types.o -o $(@)

types.o : types.F90
	gfortran -c -O2 -std=f95 types.F90
