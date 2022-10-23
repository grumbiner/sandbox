FCMDS = harmfront hcust hiter irfront iriter sigdri fanalyze t2front gauss.random tmodulate superm dcheck.deformat testing

CCMDS = harmonic1 harmonicpost harmonicquarter splice30

all : $(FCMDS) $(CCMDS)

FOBJS = demod.o detrnd.o harmim.o harmin.o readin.o signif.o tidefr.o sssum.o sumcos.o sort.o p.o pythag.o rin.o ritout.o velcmp.o yes.o filt.o ftrc.o harmrm.o harmrn.o 

#--------- Relatively unchanged below here --------------
.o: %.f
	gfortran -c $<

COPTS = -I ~/usrlocal/mmablib/include ~/usrlocal/mmablib/libombf_4.a -DLINUX -DGRIDTYPE=global_ice
#.o: %.C
#	g++ -c $(COPTS) $<

$(FCMDS) : %: %.f $(FOBJS)
	gfortran $< $(FOBJS) ../lib/linpack.a ../lib/librefblas.a ../lib/imsl.a -o $(@)

$(CCMDS) : %: %.C
	g++ $< $(COPTS) -o $(@)

clean :
	rm $(FOBJS)

distclean :
	rm $(CCMDS) $(FCMDS)


#reference: FUTIL = ../util

