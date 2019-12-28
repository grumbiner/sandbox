#   File:       oeschger.make
#   Target:     oeschger
#   Sources:    Judy:bob:writing:CO2.bw:program:advdif.v2.f
#               Judy:bob:writing:CO2.bw:program:advstr.v2.f
#               Judy:bob:writing:CO2.bw:program:oeschger.v2.f
#               Judy:bob:writing:CO2.bw:program:oestart.v2.f
#               Judy:bob:writing:CO2.bw:program:oeout.v2b.f
#               Judy:bob:writing:CO2.bw:program:theory.v2.f
#   Created:    Tuesday, May 28, 1991 15:19:05


OBJECTS = ¶
		Judy:bob:writing:CO2.bw:program:advdif.v2.f.o ¶
		Judy:bob:writing:CO2.bw:program:advstr.v2.f.o ¶
		Judy:bob:writing:CO2.bw:program:oeschger.v2.f.o ¶
		Judy:bob:writing:CO2.bw:program:oestart.v2.f.o ¶
		Judy:bob:writing:CO2.bw:program:oeout.v2b.f.o ¶
		Judy:bob:writing:CO2.bw:program:theory.v2.f.o


oeschger ÄÄ oeschger.make
	Duplicate -r -y "{FLibraries}F77mrwe.o" oeschger

oeschger ÄÄ oeschger.make {OBJECTS}
	Link -f -srt -w -t APPL -c '????' ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o oeschger
Judy:bob:writing:CO2.bw:program:advdif.v2.f.o Ä oeschger.make Judy:bob:writing:CO2.bw:program:advdif.v2.f
	 f77compiler -O Judy:bob:writing:CO2.bw:program:advdif.v2.f
Judy:bob:writing:CO2.bw:program:advstr.v2.f.o Ä oeschger.make Judy:bob:writing:CO2.bw:program:advstr.v2.f
	 f77compiler -O Judy:bob:writing:CO2.bw:program:advstr.v2.f
Judy:bob:writing:CO2.bw:program:oeschger.v2.f.o Ä oeschger.make Judy:bob:writing:CO2.bw:program:oeschger.v2.f
	 f77compiler -O Judy:bob:writing:CO2.bw:program:oeschger.v2.f
Judy:bob:writing:CO2.bw:program:oestart.v2.f.o Ä oeschger.make Judy:bob:writing:CO2.bw:program:oestart.v2.f
	 f77compiler -O Judy:bob:writing:CO2.bw:program:oestart.v2.f
Judy:bob:writing:CO2.bw:program:oeout.v2b.f.o Ä oeschger.make Judy:bob:writing:CO2.bw:program:oeout.v2b.f
	 f77compiler -O Judy:bob:writing:CO2.bw:program:oeout.v2b.f
Judy:bob:writing:CO2.bw:program:theory.v2.f.o Ä oeschger.make Judy:bob:writing:CO2.bw:program:theory.v2.f
	 f77compiler -O Judy:bob:writing:CO2.bw:program:theory.v2.f
