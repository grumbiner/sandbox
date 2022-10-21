HSOBJS = arset1.o adv2d1.o adv2d2.o arset.o convec.o init.o integ2.o laplac.o mad2d1.o mad2d2.o qstart.o source.o summer.o ucext.o unknown.o vector.opers.o
hsmain : hsmain.o $(HSOBJS)
	gfortran $< $(HSOBJS) -o $(@)

FC=gfortran
CC=gcc
CPP=g++

#Building elements
CPPOPTS = -c
COPTS = -c
%.o: %.C
	$(CPP) $(CPPOPTS) $< -o $(@)
%.o: %.c
	$(CC) $(COPTS) $< -o $(@)

FOPTS = -c
%.o: %.f
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F90
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.f90
	$(FC) $(FOPTS) $< -o $(@)


#hsmain.f:      PROGRAM hsmain
#hsmain2.f:      PROGRAM pretty4
#k74main.f:C     THIS PROGRAM IS DESIGNED TO DUPLICATE THE PROGRAM USED IN
#kill74.f:      PROGRAM Kill74
#
#adv2main.f:      PROGRAM full
#advector.f:      PROGRAM advector
#
#ltest.f:      PROGRAM ltest
#outconv2.f:      PROGRAM prepgr
#salt.f:      PROGRAM levels
#tests.f:      PROGRAM tests
#translate.f:      PROGRAM transl
