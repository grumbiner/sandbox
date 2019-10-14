pretty4 : yes.f rho.f uvtrop.f timav.f stmfc4.f reflux.f outdat.f init.f uvext.f summer.f stext.f qsext.f cshelf.f convec.f read2.f arset.f
	gfortran -O2 -ffpe-trap=underflow,denormal  *.f -o $(@)
	#gfortran -O2 -ffpe-trap=underflow,denormal,inexact *.f -o $(@)
	#gfortran -O2 *.f -o $(@)
