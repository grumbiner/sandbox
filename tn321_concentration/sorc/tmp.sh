#PBS -N amsr2_j11
#PBS -o amsr2_j11
#PBS -j oe
#PBS -A ICE-DEV
#PBS -q dev
#PBS -l walltime=0:45:00
#PBS -l select=1:ncpus=1

module load prod_envir/2.0.6
module load prod_util/2.0.14

module load intel/19.1.3.304
module load bufr_dump/1.1.2

echo zzz post module list
module list
echo zzz post module list

set -xe

export cyc=${cyc:-00}
export envir=${envir:-developer}
export seaice_amsr2_ver=${seaice_amsr2_ver:-v4.6.0}
export job=${job:-seaice_amsr2}

export HOMEseaice_amsr2=$HOME/rgdev/new_concentrations/amsr2
export DATAROOT=/lfs/h2/emc/ptmp/wx21rg

export COMROOT=$HOME/noscrub/

pid=$$

#zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

export PDY=${PDY:-20220111}
export PDYm1=${PDYm1:-20220110}
while [ $PDY -le 20230115 ]
do

  export COMOUT=$COMROOT/seaice/developer/amsr2.$PDY
  if [ ! -d $COMOUT ] ; then
    mkdir -p $COMOUT
  fi
  
  #copy+rename ice conc from $HOME/noscrub/sice/sice.$PDY/  to same output dir
  if [ -f $HOME/noscrub/sice/sice.$PDY/seaice.t00z.fill5min ] ; then
    cp -p $HOME/noscrub/sice/sice.$PDY/seaice.t00z.fill5min $COMOUT/ice.$PDY
  fi

  # avhrr
  if [ -f $HOME/noscrub/sice/sice.$PDY/land.$PDYm1 ] ; then
    mv $HOME/noscrub/sice/sice.$PDY/land.$PDYm1 $COMOUT
  fi
  if [ -f $HOME/noscrub/sice/sice.$PDY/seas.$PDYm1 ] ; then
    mv $HOME/noscrub/sice/sice.$PDY/seas.$PDYm1 $COMOUT
  fi
  
  export PDYm1=$PDY
  PDY=`expr $PDY + 1`
  PDY=`$HOME/bin/dtgfix3 $PDY`

done
