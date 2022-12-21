#QSUB -me
#QSUB -lM 2.0Mw

REF=/wd2/wd21/wd21rg/hibler
set -xe

#for pole in north south 
#Changed mind, have the various scripts decide if they care which
#  pole is involved.
#do
#  export pole

#Create the north and south hemisphere mask files, and ocean T, S 
# files.
#  cd $REF/bc
#  qsub $REF/bc/bc.jcl 

#  cd $REF/meteo
#  sleep 300; qsub $REF/meteo/met.jcl 

for pole in north south
do
  cd $REF/mpi.source
  cp icegrid.$pole icegrid.inc
  chmod 644 icegrid.inc
  make stossel
  mv stossel ../running/stossel.$pole
done
#  cd $REF/analy
#  ncargf77 icemov.f -o icemov.$pole

#done
