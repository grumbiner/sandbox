a.out : main.F90 types.mod
	gfortran main.F90 typedefs.o

types.mod : typedefs.F90
	gfortran -c $<

