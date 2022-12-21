RHOME=/wd2/wd21/wd21rg/icemodel
IHOME=/wd2/wd21/wd21rg/icemodel/mpi.source
PHOME=/wd2/wd21/wd21rg/library
OHOME=/wd2/wd21/wd21rg/others
W3=/nwprod/w3lib

LD=segldr
FC='cf77 -c'

export RHOME IHOME OHOME PHOME W3 LD FC

for pole in north south
do
  rm *.inc
  cp $IHOME/icegrid.$pole $IHOME/icegrid.inc
  chmod 644 $IHOME/icegrid.inc
  make
  mv met met.$pole
done

