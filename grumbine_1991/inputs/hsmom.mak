#FC = cf77 -c
#LD = segldr
FC=gfortran -c
LD=gfortran

hssw  : adv2d1.o adv2d2.o integ.o init.o isrc.o hsstart.o hssourc.o \
       hsmain.o lapl.o bcond.o ucext.o convec.o vadd.o arset.o\
       uvtrop.o stmfc4.o reflux.o mad2d2.o mad2d1.o ubc.o speed.o msrc.o 
	$(LD) hsmain.o adv2d1.o adv2d2.o integ.o init.o hssourc.o hsstart.o\
        isrc.o lapl.o bcond.o ucext.o convec.o vadd.o arset.o msrc.o\
       uvtrop.o stmfc4.o reflux.o mad2d2.o mad2d1.o ubc.o speed.o -o hssw
clean :
	rm *.o 

hsmain.o : hsmain.f grid.inc hsmom.mak
	$(FC) hsmain.f

adv2d1.o   : adv2d1.f grid.inc hsmom.mak
	$(FC) adv2d1.f
adv2d2.o   : adv2d2.f grid.inc hsmom.mak
	$(FC) adv2d2.f
mad2d1.o   : mad2d1.f grid.inc hsmom.mak
	$(FC) mad2d1.f
mad2d2.o   : mad2d2.f grid.inc hsmom.mak
	$(FC) mad2d2.f
integ.o : integ.f grid.inc hsmom.mak
	$(FC) integ.f
init.o : init.f grid.inc hsmom.mak
	$(FC) init.f
isrc.o : isrc.f grid.inc hsmom.mak
	$(FC) isrc.f
msrc.o : msrc.f grid.inc hsmom.mak
	$(FC) msrc.f
hssourc.o : hssourc.f grid.inc hsmom.mak
	$(FC) hssourc.f
hsstart.o : hsstart.f grid.inc hsmom.mak
	$(FC) hsstart.f
lapl.o : lapl.f grid.inc hsmom.mak
	$(FC) lapl.f
bcond.o : bcond.f grid.inc hsmom.mak
	$(FC) bcond.f
ubc.o : ubc.f grid.inc hsmom.mak
	$(FC) ubc.f
ucext.o : ucext.f grid.inc hsmom.mak
	$(FC) ucext.f
convec.o : convec.f grid.inc hsmom.mak
	$(FC) convec.f
vadd.o : vadd.f grid.inc hsmom.mak
	$(FC) vadd.f
speed.o : speed.f grid.inc hsmom.mak
	$(FC) speed.f
arset.o : arset.f grid.inc hsmom.mak
	$(FC) arset.f
uvtrop.o : uvtrop.f grid.inc hsmom.mak
	$(FC) uvtrop.f
stmfc4.o : stmfc4.f grid.inc hsmom.mak
	$(FC) stmfc4.f
reflux.o : reflux.f grid.inc hsmom.mak
	$(FC) reflux.f
