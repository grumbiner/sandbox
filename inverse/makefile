# Definitions:
FC = gfortran
FFLAGS = -c -O2
LD = gfortran

OBJS = anoise.o avger.o flag.o locdat.o mksmpl.o readat.o synth.o trimmed.o wght1.o writ.o

SSMI_OBJECTS = agree.o autocor.o blnkrm.o correl.o mkrand.o ran2.o sample.o sumx.o sumx2.o sumxy.o tridig.o veceq.o

.f.o:
	$(FC) $(FFLAGS) $<

ssmi.lib : $(SSMI_OBJECTS)
	ar rv ssmi.lib $(SSMI_OBJECTS)


#Executables:
icevary : icevary.o derivs.o scalars.o varwght.o inv.inc
	$(LD) icevary.o derivs.o scalars.o varwght.o \
		$(LIBDIR)/polarflib -o icevary
        
vremap : vremap.o
	$(LD) vremap.o -o vremap

straw : straw.o scalars.o
	$(LD) straw.o scalars.o $(LIBDIR)/polarflib -o straw

forward : for4.o
	$(LD) for4.o $(LIBDIR)/polarflib -o forward

mrand : mrand.o 
	$(LD) mrand.o $(LIBDIR)/polarflib -o mrand


bc2 : bc2.o $(OBJS) $(SSMI_OBJECTS)
	$(LD) $(@).o $(OBJS) $(SSMI_OBJECTS) -o $(@)
acor3 : acor3.o $(OBJS) $(SSMI_OBJECTS)
	$(LD) $(@).o $(OBJS) $(SSMI_OBJECTS) -o $(@)
rem5 : rem5.o $(OBJS) $(SSMI_OBJECTS)
	$(LD) $(@).o $(OBJS) $(SSMI_OBJECTS) -o $(@)

#bc2.o : bc2.f
#acor3.o : acor3.f
#rem5.o   : rem5.f



#Misc:

clean :
	rm *.o
