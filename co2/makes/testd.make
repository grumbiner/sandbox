#   File:       testd.make
#   Target:     testd
#   Sources:    HD.I40:mcgill:big.model:carbon:testdco2.f
#               HD.I40:mcgill:big.model:carbon:dco2.f
#               HD.I40:mcgill:big.model:carbon:pco2.f
#   Created:    Saturday, October 24, 1992 18:07:19


OBJECTS = ¶
		HD.I40:mcgill:big.model:carbon:testdco2.f.o ¶
		HD.I40:mcgill:big.model:carbon:cco2.f.o ¶
		HD.I40:mcgill:big.model:carbon:dco2.f.o ¶
		HD.I40:mcgill:big.model:carbon:pco2.f.o



FFLAGS = -q

"testd" ÄÄ "testd".make
	Duplicate -r -y "{FLibraries}F77mrwe.o" "testd"

testd ÄÄ testd.make {OBJECTS}
	Link -f -model far -t APPL -c rmg3 ¶
		{OBJECTS} ¶
		"{Libraries}"Runtime.o ¶
		"{Libraries}"Interface.o ¶
		"{FLibraries}"F77mrwe.o ¶
		"{FLibraries}"frt0.o ¶
		"{FLibraries}"f77io.o ¶
		"{FLibraries}"f77math.o ¶
		-o testd
HD.I40:mcgill:big.model:carbon:testdco2.f.o Ä testd.make HD.I40:mcgill:big.model:carbon:testdco2.f
	 f77compiler {FFLAGS} HD.I40:mcgill:big.model:carbon:testdco2.f
HD.I40:mcgill:big.model:carbon:dco2.f.o Ä testd.make HD.I40:mcgill:big.model:carbon:dco2.f
	 f77compiler {FFLAGS} HD.I40:mcgill:big.model:carbon:dco2.f
HD.I40:mcgill:big.model:carbon:cco2.f.o Ä testd.make HD.I40:mcgill:big.model:carbon:cco2.f
	 f77compiler {FFLAGS} HD.I40:mcgill:big.model:carbon:cco2.f
HD.I40:mcgill:big.model:carbon:pco2.f.o Ä testd.make HD.I40:mcgill:big.model:carbon:pco2.f
	 f77compiler {FFLAGS} HD.I40:mcgill:big.model:carbon:pco2.f
