# These are now set by calling environment:
#COPTS= -Wall  -O2 -DLINUX 
#MMAB_INC=$(BASE)/mmablib/$(mmablib_ver)/include/

all : seaice_midpoints 

seaice_midpoints : midpoints.C centroid.C
	$(CC) $(COPTS) midpoints.C -I $(MMAB_INC) -o $(@) $(LIBS) -l ombf_4 -l imf 
