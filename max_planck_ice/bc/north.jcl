#QSUB -lT 100
#QSUB -lM 2Mw
#QSUB -me

RHOME=/wd2/wd21/wd21rg
IHOME=$RHOME/hibler/mpi.source

cd $RHOME/hibler/bc

for pole in north 
do

  chmod 644 ${IHOME}/icegrid.inc
  cp ${IHOME}/icegrid.$pole ${IHOME}/icegrid.inc
  chmod 644 ${IHOME}/icegrid.inc

  make clobber
  make

# Create the mask file
  assign -a $RHOME/data/topographies/zmask -Fcos -Nibm fort.10
  touch maskin
#  '3' selects that we're working with the zmask 1x1 file.
  echo "3" > maskin
  rm maskfull maskout
  time masker < maskin > maskrun
  mv maskfull ../forothers/mask.$pole

# Create the file listing latitudes and longitudes on the ice grid.
  listpts > ../forothers/list.$pole
  mv latlongpts ../forothers/latlong.$pole

# Create the ocean t, s files
  cp maskout MASK
  rm tsdeep tsshal ????OUT 
  time ocean < ${pole}.oin > oceanout
  cp MASK tsdeep tsshal ../running/$pole
  mv MASK tsdeep tsshal oceanout outliers $pole

done 

chmod 644 ../forothers/*
