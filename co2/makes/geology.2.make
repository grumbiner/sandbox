#   File:       geology.2.make
#   Target:     geology.2
#   Sources:    aabw.f
#               advdif.f
#               advstr.f
#               atmos.f
#               biosrc.f
#               bwsrc.f
#               cco2.f
#               gasses.f
#               geolog.f
#               oemain.f
#               oeout.f
#               oestart.f
#               ssink.f
#               theory.f
#               upwell.f
#   Created:    Tuesday, October 29, 1991 14:36:16


OBJECTS = ¶
		aabw.f.o ¶
		advdif.f.o ¶
		advstr.f.o ¶
		atmos.f.o ¶
		biosrc.f.o ¶
		bwsrc.f.o ¶
		cco2.f.o ¶
		gasses.f.o ¶
		geolog.f.o ¶
		oemain.f.o ¶
		oeout.f.o ¶
		oestart.f.o ¶
		ssink.f.o ¶
		theory.f.o ¶
		upwell.f.o

FFLAGS = -q -N11 -N8 -C -F -mf -N13 -O -N32

"geology.2" ÄÄ "geology.2".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "geology.2"

geology.2 ÄÄ geology.2.make {OBJECTS}
	Link -f -model far -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o geology.2
aabw.f.o Ä geology.2.make aabw.f
	 f77compiler {FFLAGS} aabw.f
advdif.f.o Ä geology.2.make advdif.f
	 f77compiler {FFLAGS} advdif.f
advstr.f.o Ä geology.2.make advstr.f
	 f77compiler {FFLAGS} advstr.f
atmos.f.o Ä geology.2.make atmos.f
	 f77compiler {FFLAGS} atmos.f
biosrc.f.o Ä geology.2.make biosrc.f
	 f77compiler {FFLAGS} biosrc.f
bwsrc.f.o Ä geology.2.make bwsrc.f
	 f77compiler {FFLAGS} bwsrc.f
cco2.f.o Ä geology.2.make cco2.f
	 f77compiler {FFLAGS} cco2.f
gasses.f.o Ä geology.2.make gasses.f
	 f77compiler {FFLAGS} gasses.f
geolog.f.o Ä geology.2.make geolog.f
	 f77compiler {FFLAGS} geolog.f
oemain.f.o Ä geology.2.make oemain.f
	 f77compiler {FFLAGS} oemain.f
oeout.f.o Ä geology.2.make oeout.f
	 f77compiler {FFLAGS} oeout.f
oestart.f.o Ä geology.2.make oestart.f
	 f77compiler {FFLAGS} oestart.f
ssink.f.o Ä geology.2.make ssink.f
	 f77compiler {FFLAGS} ssink.f
theory.f.o Ä geology.2.make theory.f
	 f77compiler {FFLAGS} theory.f
upwell.f.o Ä geology.2.make upwell.f
	 f77compiler {FFLAGS} upwell.f
