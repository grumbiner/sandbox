MMABLIB_RG=/u/wx21rg/rgdev/mmablib/sorc/omb
MMABLIB_OPS=/nwtest/lib/sorc/omb/

for fn in GRIBIT.f VCC.f arcdis.f bsslz1.f cfs.bin.full cfs.bini cfs.binj cfsread.f correl.f fit.f gaulat.f iagree.f makelibombC.sh makelibombF.sh mapll.c mapxy.c mapxy.f sumx.f sumx2.f sumxy.f tfreez.f vectorize.f w3ft01.f wdir.f wmoout.f include/amsr.h include/amsrice.h include/avhrr.h include/buoy.h include/clib.h include/clib.inc include/clib4.h include/clib8.h include/cofs.h include/color.h include/eta.h include/from.h include/fromall.h include/fromcfs.h include/gaussian.h include/genes.h include/grib.h include/grid3.h include/grid_base.h include/grid_math.h include/gshhs.h include/icegrid.global include/icegrid.global5min include/icegrid.north include/icegrid.north12 include/icegrid.south include/icegrid.south12 include/icegrids.h include/icessmi.h include/lambert.h include/locale.inc include/metric.h include/mvector.h include/ncepgrids.h include/params.h include/points.h include/resops.h include/ssmi.h include/ssmiclass.h include/ssmis.h
do
  x=`cmp -l $MMABLIB_RG/$fn $MMABLIB_OPS/$fn | wc -l`
  echo $fn has $x diffs
done
