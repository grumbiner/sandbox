alpha : a.F90 modernize.o harmonic.mod
	gfortran -std=f2003 a.F90 modernize.o -o alpha

harmonic.mod : modernize.o
	gfortran -c -std=f2003 modernize.F90
	
modernize.o : modernize.F90
	gfortran -c -std=f2003 modernize.F90

clean :
	rm *.o *.mod alpha
