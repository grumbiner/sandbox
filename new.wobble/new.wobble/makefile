
all : main

main : main.o extract.o harmrm.o 
	gfortran $(@).o extract.o harmrm.o -L $(HOME)/usrlocal/lib -l lapack -l refblas
