#   File:       locate.make
#   Target:     locate
#   Sources:    Judy:bob:cd.work:locate.f
#               Judy:bob:cd.work:mapll.f
#               Judy:bob:cd.work:mapxy.f
#   Created:    Thursday, October 24, 1991 11:45:32


OBJECTS = ¶
		Judy:bob:cd.work:locate.f.o ¶
		Judy:bob:cd.work:mapll.f.o ¶
		Judy:bob:cd.work:mapxy.f.o



FFLAGS = -q -O -A -N11 -N8 -C -F -mf -N13

"locate" ÄÄ "locate".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "locate"

locate ÄÄ locate.make {OBJECTS}
	Link -f -model far -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o locate
Judy:bob:cd.work:locate.f.o Ä locate.make Judy:bob:cd.work:locate.f
	 f77compiler {FFLAGS} Judy:bob:cd.work:locate.f
Judy:bob:cd.work:mapll.f.o Ä locate.make Judy:bob:cd.work:mapll.f
	 f77compiler {FFLAGS} Judy:bob:cd.work:mapll.f
Judy:bob:cd.work:mapxy.f.o Ä locate.make Judy:bob:cd.work:mapxy.f
	 f77compiler {FFLAGS} Judy:bob:cd.work:mapxy.f
