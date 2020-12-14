SDIR=/wd2/wd21/wd21rg/icemodel/mpi.source
cd $SDIR
for pole in north south
do
  export pole 
  solo $pole
done
