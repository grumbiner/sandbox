all : plot aerodynamics

plot : plot.C
	g++ -Wall -O2 -DLINUX $< -I ~/usrlocal/mmablib/include  -DNX=1024 -DNY=256 -o $(@)

aerodynamics : aerodynamics.F90
	gfortran -O2 $< -o $(@)
