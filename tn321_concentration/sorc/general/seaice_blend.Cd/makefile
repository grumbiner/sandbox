#Robert Grumbine 27 May 2016

#Linux 

include ../makeall.mk


#Should be no changes below this point---------------------------------

seaice_blend : blend.o
	$(CPPLD) $(CPPLDFLAGS) blend.o $(MMAB_LIBF4) -o  seaice_blend

blend.o : blend.C 
	$(CPP) $(CPPOPTS) -I $(MMAB_INC) -DTEAM2 blend.C 

