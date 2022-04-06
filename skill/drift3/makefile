all: avg2 iabpcheck score

.C : ~/svn/mmab/v3.4.2/include/buoy.h 
	g++  -c -Wall -O2 -I ~/svn/mmab/v3.4.2/include $< -DLINUX -Wno-deprecated

.o :
	g++ $<  ~/svn/mmab/v3.4.2/libombf_4.a -o $(@)
