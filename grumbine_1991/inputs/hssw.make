#   File:       hssw.make
#   Target:     hssw
#   Sources:    :adv:ADV2D1.F
#               :adv:ADV2D2.F
#               :adv:ARSET.F
#               :adv:BCOND.F
#               :adv:CONVEC.F
#               :adv:HSMAIN.F
#               :adv:HSSOURC.F
#               :adv:HSSTART.F
#               :adv:INIT.F
#               :adv:INTEG.F
#               :adv:ISRC.F
#               :adv:LAPL.F
#               :adv:REFLUX.F
#               :adv:STMFC4.F
#               :adv:UCEXT.F
#               :adv:UVTROP.F
#               :adv:VADD.F
#   Created:    Monday, December 7, 1992 22:19:49


OBJECTS = ¶
		:adv:ADV2D1.F.o ¶
		:adv:ADV2D2.F.o ¶
		:adv:ARSET.F.o ¶
		:adv:BCOND.F.o ¶
		:adv:CONVEC.F.o ¶
		:adv:HSMAIN.F.o ¶
		:adv:HSSOURC.F.o ¶
		:adv:HSSTART.F.o ¶
		:adv:INIT.F.o ¶
		:adv:INTEG.F.o ¶
		:adv:ISRC.F.o ¶
		:adv:LAPL.F.o ¶
		:adv:REFLUX.F.o ¶
		:adv:STMFC4.F.o ¶
		:adv:UCEXT.F.o ¶
		:adv:UVTROP.F.o ¶
		:adv:VADD.F.o



FFLAGS = -q

"hssw" ÄÄ "hssw".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "hssw"

hssw ÄÄ hssw.make {OBJECTS}
	Link -f -model far -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o hssw
:adv:ADV2D1.F.o Ä hssw.make :adv:ADV2D1.F
	 f77compiler {FFLAGS} :adv:ADV2D1.F
:adv:ADV2D2.F.o Ä hssw.make :adv:ADV2D2.F
	 f77compiler {FFLAGS} :adv:ADV2D2.F
:adv:ARSET.F.o Ä hssw.make :adv:ARSET.F
	 f77compiler {FFLAGS} :adv:ARSET.F
:adv:BCOND.F.o Ä hssw.make :adv:BCOND.F
	 f77compiler {FFLAGS} :adv:BCOND.F
:adv:CONVEC.F.o Ä hssw.make :adv:CONVEC.F
	 f77compiler {FFLAGS} :adv:CONVEC.F
:adv:HSMAIN.F.o Ä hssw.make :adv:HSMAIN.F
	 f77compiler {FFLAGS} :adv:HSMAIN.F
:adv:HSSOURC.F.o Ä hssw.make :adv:HSSOURC.F
	 f77compiler {FFLAGS} :adv:HSSOURC.F
:adv:HSSTART.F.o Ä hssw.make :adv:HSSTART.F
	 f77compiler {FFLAGS} :adv:HSSTART.F
:adv:INIT.F.o Ä hssw.make :adv:INIT.F
	 f77compiler {FFLAGS} :adv:INIT.F
:adv:INTEG.F.o Ä hssw.make :adv:INTEG.F
	 f77compiler {FFLAGS} :adv:INTEG.F
:adv:ISRC.F.o Ä hssw.make :adv:ISRC.F
	 f77compiler {FFLAGS} :adv:ISRC.F
:adv:LAPL.F.o Ä hssw.make :adv:LAPL.F
	 f77compiler {FFLAGS} :adv:LAPL.F
:adv:REFLUX.F.o Ä hssw.make :adv:REFLUX.F
	 f77compiler {FFLAGS} :adv:REFLUX.F
:adv:STMFC4.F.o Ä hssw.make :adv:STMFC4.F
	 f77compiler {FFLAGS} :adv:STMFC4.F
:adv:UCEXT.F.o Ä hssw.make :adv:UCEXT.F
	 f77compiler {FFLAGS} :adv:UCEXT.F
:adv:UVTROP.F.o Ä hssw.make :adv:UVTROP.F
	 f77compiler {FFLAGS} :adv:UVTROP.F
:adv:VADD.F.o Ä hssw.make :adv:VADD.F
	 f77compiler {FFLAGS} :adv:VADD.F
