#IBM AIX
CLD=ncepxlC 
RPP=ncepxlC -c -O2 -I$(INCDIR) -DTEAM2 -DHIRES -DIBM -DCPLUS 
INCDIR=/nwprod/lib/sorc/omb/include 
LIBS = -L/nwprod/lib/ -lombc_4 -lombf_4 -lw3_4 -lm

#linux 
CLD=g++ -O2 -Wall -DTEAM2 -DLINUX -DCPLUS
RPP=g++ -c -Wall -O2 -I $(INCDIR) -DTEAM2 -DLINUX -DCPLUS
INCDIR=$(BASE)/mmablib_v3.1.wcoss/include
LIBS=$(BASE)/mmablib_v3.1.wcoss/libombc_4.a $(BASE)/mmablib_v3.1.wcoss/libombf_4.a -lm
#Should be no changes below this point---------------------------------

seaice_seaiamsr : amsr.main.o
	$(CLD) amsr.main.o $(LIBS) -o seaice_seaiamsr

amsr.main.o : amsr.main.C amsr_getfld.C amsr_hires.C amsr_team2.C amsr_icetools.C pole_fill.C amsr_process_bufr.C
	$(RPP) amsr.main.C 

