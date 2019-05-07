FC = gfortran -c -O2 
LD = gfortran

predict : predmain.o initpred.o grower.o iceprd.o temper.o Makefile 
	$(LD) predmain.o initpred.o grower.o iceprd.o temper.o -o predict
icethk  : icethk.o initpred.o grower.o iceprd.o temper.o Makefile 
	$(LD) icethk.o initpred.o grower.o iceprd.o temper.o -o icethk
clean :
	rm *.o growth predictable temps debugs 

predmain.o   : predmain.f Makefile
	$(FC) predmain.f
icethk.o   : icethk.f Makefile
	$(FC) icethk.f
initpred.o   : initpred.f Makefile
	$(FC) initpred.f
grower.o : grower.f Makefile
	$(FC) grower.f
iceprd.o : iceprd.f Makefile
	$(FC) iceprd.f
temper.o : temper.f Makefile
	$(FC) temper.f
