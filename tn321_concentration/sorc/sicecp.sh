#PBS -N sice
#PBS -o sice
#PBS -j oe
#PBS -A ICE-DEV
#PBS -q dev
#PBS -l walltime=8:00:00
#PBS -l select=1:ncpus=1

export PDY=${PDY:-20200101}
export PDYm1=${PDYm1:-20191231}
export end=20230326

module load prod_envir/2.0.6
module load prod_util/2.0.14

module load intel/19.1.3.304
module load bufr_dump/1.1.2

echo zzz post module list
module list
echo zzz post module list

#set -x

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
    if [ -f  $HOME/noscrub/sice/sice.$PDY/seaice.t00z.fill5min -a ! -f  $COMOUT/ice.$PDY ]; then
      echo zzz cp $PDY
      cp -p $HOME/noscrub/sice/sice.$PDY/seaice.t00z.fill5min $COMOUT/ice.$PDY
    fi
    # avhrr
    if [ -f $HOME/noscrub/sice/sice.$PDY/land.$PDYm1 ] ; then
      echo zzz mv land $PDY
      mv $HOME/noscrub/sice/sice.$PDY/land.$PDYm1 $COMOUT
    fi
    if [ -f $HOME/noscrub/sice/sice.$PDY/seas.$PDYm1 ] ; then
      echo zzz mv seas $PDY
      mv $HOME/noscrub/sice/sice.$PDY/seas.$PDYm1 $COMOUT
    fi
  fi
  
  PDYm1=$PDY
  PDY=`expr $PDY + 1`
  PDY=`$HOME/bin/dtgfix3 $PDY`

done
