#   File:       ssmiice.make
#   Target:     ssmiice
#   Sources:    ssmiice.f usetie.f getice.f
#   Created:    Thursday, October 24, 1991 11:50:15


OBJECTS = ssmiice.f.o usetie.f.o getice.f.o



FFLAGS = -q -O -A -N11 -N8 -C -F -mf -N13

"ssmiice" ÄÄ "ssmiice".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "ssmiice"

ssmiice ÄÄ ssmiice.make {OBJECTS}
	Link -f -model far -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o ssmiice
ssmiice.f.o Ä ssmiice.make ssmiice.f
	 f77compiler {FFLAGS} ssmiice.f
usetie.f.o Ä ssmiice.make usetie.f
	 f77compiler {FFLAGS} usetie.f
getice.f.o Ä ssmiice.make getice.f
	 f77compiler {FFLAGS} getice.f
