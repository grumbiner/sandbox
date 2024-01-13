#PBS -N amsrbufra
#PBS -o amsrbufra
#PBS -j oe
#PBS -A ICE-DEV
#PBS -q dev
#PBS -l walltime=8:00:00
#PBS -l select=1:ncpus=1

export PDY=${PDY:-20210102}
export PDYm1=${PDYm1:-20220101}
#export end=20220731
export end=20230415

module load prod_envir/2.0.6
module load prod_util/2.0.14

module load intel/19.1.3.304
module load bufr_dump/1.1.2

echo zzz post module list
module list
echo zzz post module list

set -x

export cyc=${cyc:-00}
export envir=${envir:-developer}
export seaice_amsr2_ver=${seaice_amsr2_ver:-v4.6.0}
export job=${job:-seaice_amsr2}

export HOMEseaice_amsr2=$HOME/rgdev/new_concentrations/amsr2
export DATAROOT=/lfs/h2/emc/ptmp/wx21rg

export COMROOT=$HOME/noscrub/
export KEEPDATA="YES"

# For dumpjb:
export DCOMROOT=$HOME/noscrub/satellites/prod
echo zzz DCOMROOT= $DCOMROOT

pid=$$
export DATA=$DATAROOT/${job}.${pid}
mkdir -p $DATA
cd $DATA
#For dumpjb
export TMPDIR=$DATA

#zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

echo zzz begin main while loop

while [ $PDY -le $end ]
do

  export COMOUT=$COMROOT/seaice/developer/amsr2.$PDY
  if [ ! -d $COMOUT ] ; then
    mkdir -p $COMOUT
  fi
  
  #zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
  ##copy+rename ice conc from $HOME/noscrub/sice/sice.$PDY/  to same output dir
  if [ ! -f  $COMOUT/ice.$PDY ] ; then
    for f in seaice.t00z.fill5min  land.$PDYm1 seas.$PDYm1
    do   
      if [ -f $HOME/noscrub/sice/sice.$PDY/$f ] ; then
        mv $HOME/noscrub/sice/sice.$PDY/$f $COMOUT/
      fi
    done
    mv $COMOUT/seaice.t00z.fill5min $COMOUT/ice.$PDY 
  fi
  
  #amsr2 bufr to binary
  if [ ! -f $COMOUT/amsr2.$PDY ] ; then
    echo zzz getamsr to noscrub/seaice/developer/amsr2.$PDY

    time $DUMPJB ${PDY}00 12 amsr2
  
    ln -sf amsr2.ibm fort.11
    time $HOME/rgdev/new_concentrations/amsr2/exec/seaice_amsrbufr
    mv fort.52 amsr2.$PDY
    cp  amsr2.$PDY $COMOUT
  fi
  
  
  PDYm1=$PDY
  PDY=`expr $PDY + 1`
  PDY=`$HOME/bin/dtgfix3 $PDY`

done

cd ..
rm -rf $DATA
