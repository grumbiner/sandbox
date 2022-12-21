#!/bin/sh

EXDIR=/u/wx21rg/nfsuser/icemodel/execs
OUTDIR=/emc1/ice/model/hcst
RUNDIR=/emc1/ice/model/tmp
export EXDIR OUTDIR RUNDIR

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

set -x

tag=20001001
while [ $tag -le 20010930 ]
do
for pole in north south 
do

    for hh in 00 06 12 18
    do

      if [ -f ${OUTDIR}/$pole/${tag}/f${hh}.tar ] ; then
        rm fort.?? conc thick vels hml sml tml atm.flux oce.flux 
        tar xvf ${OUTDIR}/$pole/${tag}/f${hh}.tar

        ln -f conc fort.10
        ln -f thick fort.11
        ln -f hml  fort.12
        ln -f atm.flux fort.13
        ln -f oce.flux fort.14
        ln -f sml fort.15
        ln -f tml fort.16
        ln -f vels fort.20
  
#Note, must include proper century
        echo $tag | cut -c3-4 > gin
        echo $tag | cut -c5-6 >> gin
        echo $tag | cut -c7-8 >> gin
        echo $hh >> gin
        echo 6 >> gin
  
#       Construct the files 
        cat gin | $EXDIR/icegrib.$pole
        mv fort.51 ${OUTDIR}/$pole/$tag/${pole}.${tag}${hh}.grb
        mv FORT.16 ${OUTDIR}/$pole/$tag/F16.$hh
        mv FORT.18 ${OUTDIR}/$pole/$tag/F18.$hh

        rm ${OUTDIR}/$pole/${tag}/f${hh}.tar
 
      fi 
    done
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done   
