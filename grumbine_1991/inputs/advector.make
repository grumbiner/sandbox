#   File:       advector.make
#   Target:     advector
#   Sources:    ADV2D1.F ADV2D2.F ADVECTOR.F
#   Created:    Sunday, June 6, 1993 21:05:00


OBJECTS = ADV2D1.F.o ADV2D2.F.o ADVECTOR.F.o bc.f.o

FFLAGS = -q

"advector" ÄÄ "advector".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "advector"

advector ÄÄ advector.make {OBJECTS}
	Link -f -model far -t APPL -c '????' ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o advector
ADV2D1.F.o Ä advector.make ADV2D1.F
	 f77compiler {FFLAGS} ADV2D1.F
bc.f.o Ä advector.make bc.f
	 f77compiler {FFLAGS} bc.f
ADV2D2.F.o Ä advector.make ADV2D2.F
	 f77compiler {FFLAGS} ADV2D2.F
ADVECTOR.F.o Ä advector.make ADVECTOR.F
	 f77compiler {FFLAGS} ADVECTOR.F