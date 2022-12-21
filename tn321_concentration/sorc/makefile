include ../makeall.mk

        $(CPP) $(CPPOPTS)  -DGRIDTYPE=great_lakes_wave  $< -o $(@)

FFLAGS =        -O3 -convert big_endian -assume byterecl -real-size 64 -std90 -pg
CFLAGS =        -O2 -DLINUX -std=c89 -pg

%.o: %.C        
        $(CPP) $(CPPOPTS) $< -o $(@)


$(CMD):         $(OBJS)
        $(FLD) $(FLDFLAGS) -o $(@) $(OBJS) $(LIBS)


clean:
        -rm -f $(OBJS)

clobber:        clean
        -rm -f $(CMD) $(CMD).prof
