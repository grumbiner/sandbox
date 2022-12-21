all : shallow


shallow : shallow.C
	mpic++ -Wall -O2 -I /Users/rmg3/usrlocal/mmablib/include $< -o $(@) 
	#g++ -cpp -DSCALE=5 -I /Users/rmg3/usrlocal/mmablib/include -I ~/usrlocal/include $(<) -o $(@) 
	#g++ -cpp -DSCALE=5 -I /Users/rmg3/usrlocal/mmab/v3.4.3/include -I /usr/mpi/gcc/openmpi-1.4.6/include shallow.C -o shallow -L /usr/mpi/gcc/openmpi-1.4.6/lib64 -lmpi_cxx
	#mpCC -DSCALE=12 -I /u/Robert.Grumbine/save/mmablib_v3.3.1/mmablib/include shallow.C -o shallow 

