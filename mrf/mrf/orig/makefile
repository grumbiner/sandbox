SHELL=	/bin/sh
R1=	12628
R2=	6228
R3=	6228d
R4=	6228t
SDIR=	/wd2/wd20/wd20mi/f95/source2
BDIR=	/wd2/wd20/wd20mi/f95/source2/common
DDIR=	/wd2/wd20/wd20mi/f95/source2/docblock
CDIR=	/wd2/wd20/wd20mi/f95/source2/constant
LIBS=	-l/nwprod/w3lib
f$(R1).x:	f$(R1).o
	segldr -o f$(R1).x f$(R1).o $(LIBS)
f$(R1).o:	f$(R1).f
	cf77 -c -Zp -Wd"-e8 -das" -Wf"-em -o aggress" f$(R1).f
f$(R1).f:	
	echo ' %INCLUDE DC$(R1)O; %INCLUDE INCFCST;' >f$(R1).mri
	>f$(R1).mrp
	>f$(R1).f
	>f$(R1).mrt
	echo $(SDIR)/ >>f$(R1).mrp
	echo $(BDIR)/ >>f$(R1).mrp
	echo $(DDIR)/ >>f$(R1).mrp
	echo $(CDIR)/ >>f$(R1).mrp
	echo f$(R1).mri >>f$(R1).mrt
	echo f$(R1).f >>f$(R1).mrt
	echo f$(R1).mrp >>f$(R1).mrt
	/wd2/wd23/wd23ys/mjrfp/fortprepc40 <f$(R1).mrt >f$(R1).mre 2>&1
	rm f$(R1).mri f$(R1).mrp f$(R1).mrt f$(R1).mre
f$(R2).x:	f$(R2).o
	segldr -o f$(R2).x f$(R2).o $(LIBS)
f$(R2).o:	f$(R2).f
	cf77 -c -Zp -Wd"-e8 -das" -Wf"-em -o aggress" f$(R2).f
f$(R2).f:	
	echo ' %INCLUDE DC$(R2)O; %INCLUDE INCFCST;' >f$(R2).mri
	>f$(R2).mrp
	>f$(R2).f
	>f$(R2).mrt
	echo $(SDIR)/ >>f$(R2).mrp
	echo $(BDIR)/ >>f$(R2).mrp
	echo $(DDIR)/ >>f$(R2).mrp
	echo $(CDIR)/ >>f$(R2).mrp
	echo f$(R2).mri >>f$(R2).mrt
	echo f$(R2).f >>f$(R2).mrt
	echo f$(R2).mrp >>f$(R2).mrt
	/wd2/wd23/wd23ys/mjrfp/fortprepc40 <f$(R2).mrt >f$(R2).mre 2>&1
	rm f$(R2).mri f$(R2).mrp f$(R2).mrt f$(R2).mre
f$(R3).x:	f$(R3).o
	segldr -o f$(R3).x f$(R3).o $(LIBS)
f$(R3).o:	f$(R3).f
	cf77 -c -Zp -Wd"-e8 -das" -Wf"-em -o aggress" f$(R3).f
f$(R3).f:	
	echo ' %INCLUDE DC$(R3); %INCLUDE INCFCST;' >f$(R3).mri
	>f$(R3).mrp
	>f$(R3).f
	>f$(R3).mrt
	echo $(SDIR)/ >>f$(R3).mrp
	echo $(BDIR)/ >>f$(R3).mrp
	echo $(DDIR)/ >>f$(R3).mrp
	echo $(CDIR)/ >>f$(R3).mrp
	echo f$(R3).mri >>f$(R3).mrt
	echo f$(R3).f >>f$(R3).mrt
	echo f$(R3).mrp >>f$(R3).mrt
	/wd2/wd23/wd23ys/mjrfp/fortprepc40 <f$(R3).mrt >f$(R3).mre 2>&1
	rm f$(R3).mri f$(R3).mrp f$(R3).mrt f$(R3).mre
f$(R4).x:	f$(R4).o
	segldr -o f$(R4).x f$(R4).o $(LIBS)
f$(R4).o:	f$(R4).f
	cf77 -c -Zp -Wd"-e8 -das" -Wf"-em -o aggress" f$(R4).f
f$(R4).f:	
	echo ' %INCLUDE DC$(R4); %INCLUDE INCFCST;' >f$(R4).mri
	>f$(R4).mrp
	>f$(R4).f
	>f$(R4).mrt
	echo $(SDIR)/ >>f$(R4).mrp
	echo $(BDIR)/ >>f$(R4).mrp
	echo $(DDIR)/ >>f$(R4).mrp
	echo $(CDIR)/ >>f$(R4).mrp
	echo f$(R4).mri >>f$(R4).mrt
	echo f$(R4).f >>f$(R4).mrt
	echo f$(R4).mrp >>f$(R4).mrt
	/wd2/wd23/wd23ys/mjrfp/fortprepc40 <f$(R4).mrt >f$(R4).mre 2>&1
	rm f$(R4).mri f$(R4).mrp f$(R4).mrt f$(R4).mre
