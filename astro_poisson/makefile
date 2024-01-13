#!/bin/sh

SRCS = main.F90 poisson.F90 divergence.F90 advect.F90 integral.F90 
OBJS = main.o poisson.o divergence.o advect.o integral.o 

main : $(OBJS)
	gfortran -O2 main.o poisson.o divergence.o advect.o integral.o -o $(@)

$(OBJS) : $(SRCS)
	gfortran -c -O2 $(SRCS)

.F90 :
	gfortran -c -O2 $<

.o :
	gfortran $< -o $(@)

