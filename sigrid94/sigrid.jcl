#QSUB -lT  100
#QSUB -lM  2Mw
#QSUB -o   arctic.si
#QSUB -me

cd /reanl2/ice/arctic 

RHOME=/wd2/wd21/wd21rg/dataprogs/sigrid

DATAW=/reanl2/ice/arctic_west
DATAE=/reanl2/ice/arctic_east

BASEW=900411
BASEE=900412

set -xe

k=8
while [ $k -le 9 ]; do 
# Decode sigrid information on to lat-long-conc values.
  ${RHOME}/arcticw ${DATAW}/${BASEW}.${k} > westout.$k
  ${RHOME}/arctice ${DATAE}/${BASEE}.${k} > eastout.$k

# Interpolate on to regular 1/4 by 1/4 grid.
  tag=`head -1 westout.$k`
  echo $tag
  ${RHOME}/pgrida eice.$tag wice.$tag nice.$tag > nout.$k 

# Interpolate on to regular 1 by 1 grid.
  assign -a nice.$tag -s unblocked fort.10
  assign -a fice.$tag -s unblocked fort.11
  ${RHOME}/onea

# Interpolate from the quarter degree grid to bg polar grid.
  ${RHOME}/sibga eice.$tag wice.$tag bgice.$tag

# Clean up -- remove certain files, retain debugging files for now.
#  rm eice.$tag wice.$tag nice.$tag 
  mv fice.$tag arctic.$tag

  k=`expr $k + 1 `
done
