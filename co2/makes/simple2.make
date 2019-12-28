#   File:       simple2.make
#   Target:     simple2
#   Sources:    derivs.f fluxes.f rk4.f rkchem.f
#   Created:    Sunday, October 25, 1992 21:30:22

OBJECTS = derivs.f.o fluxes.f.o rk4.f.o rkchem.f.o dco2.f.o pco2.f.o

FFLAGS = -N11 -N8 -C -F -mf -N13 -c

"simple2" ÄÄ "simple2".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "simple2"

simple2 ÄÄ simple2.make {OBJECTS}
	Link -f -model far -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o simple2
derivs.f.o Ä simple2.make derivs.f
	 f77compiler {FFLAGS} derivs.f
fluxes.f.o Ä simple2.make fluxes.f bwhist.inc
	 f77compiler {FFLAGS} fluxes.f
rk4.f.o Ä simple2.make rk4.f
	 f77compiler {FFLAGS} rk4.f
dco2.f.o Ä simple2.make dco2.f
	 f77compiler {FFLAGS} dco2.f
pco2.f.o Ä simple2.make pco2.f
	 f77compiler {FFLAGS} pco2.f
rkchem.f.o Ä simple2.make rkchem.f
	 f77compiler {FFLAGS} rkchem.f
