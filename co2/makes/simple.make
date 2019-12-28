#   File:       simple.make
#   Target:     simple
#   Sources:    Judy:bob:writing:paleo.bw:simple.chem:derivs.f
#               Judy:bob:writing:paleo.bw:simple.chem:fluxes.f
#               Judy:bob:writing:paleo.bw:simple.chem:rk4.f
#               Judy:bob:writing:paleo.bw:simple.chem:rkchem.f
#   Created:    Thursday, September 19, 1991 16:11:27


OBJECTS = ¶
		Judy:bob:writing:paleo.bw:simple.chem:derivs.f.o ¶
		Judy:bob:writing:paleo.bw:simple.chem:fluxes.f.o ¶
		Judy:bob:writing:paleo.bw:simple.chem:rk4.f.o ¶
		Judy:bob:writing:paleo.bw:simple.chem:rkchem.f.o

FFLAGS = -O -N11 -N8 -C -F -mf -N13 -ad 4 -c

simple ÄÄ simple.make
	Duplicate -r -y "{FLibraries}F77mrwe.o" simple

simple ÄÄ simple.make {OBJECTS}
	Link -f -srt -w -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o simple
Judy:bob:writing:paleo.bw:simple.chem:derivs.f.o Ä simple.make Judy:bob:writing:paleo.bw:simple.chem:derivs.f
	 F77 {FFLAGS} Judy:bob:writing:paleo.bw:simple.chem:derivs.f
Judy:bob:writing:paleo.bw:simple.chem:fluxes.f.o Ä simple.make Judy:bob:writing:paleo.bw:simple.chem:fluxes.f Judy:bob:writing:paleo.bw:simple.chem:bwhist.inc
	 F77 {FFLAGS} Judy:bob:writing:paleo.bw:simple.chem:fluxes.f
Judy:bob:writing:paleo.bw:simple.chem:rk4.f.o Ä simple.make Judy:bob:writing:paleo.bw:simple.chem:rk4.f
	 F77 {FFLAGS} Judy:bob:writing:paleo.bw:simple.chem:rk4.f
Judy:bob:writing:paleo.bw:simple.chem:rkchem.f.o Ä simple.make Judy:bob:writing:paleo.bw:simple.chem:rkchem.f
	 F77 {FFLAGS} Judy:bob:writing:paleo.bw:simple.chem:rkchem.f
