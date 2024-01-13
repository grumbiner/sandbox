#!/bin/bash --login
#BSUB -J drift_catchalld
#BSUB -q "dev"
#BSUB -P RTO-T2O
#BSUB -W 6:59
#BSUB -o drift.out.%J
#BSUB -e drift.err.%J
#BSUB -R "affinity[core(1)]"
#BSUB -R "rusage[mem=128]"

#set -x
set -e

#. $MODULESHOME/init/bash

module purge
## Phase 3
#module load EnvVars/1.0.3 
#module load prod_envir/1.1.0
#module load ips/18.0.5.274 impi/18.0.1
#module load grib_util/1.1.1
#module load prod_util/1.1.0
#module load util_shared/1.1.2 #a guess
#module load w3nco/2.0.6 w3emc/2.3.0
#module load bufr/11.3.1 bacio/2.0.2
##for sidfex
#module load python/3.6.3
## Wcoss2
module load craype-x86-rome
module load libfabric/1.11.0.0
module load craype-network-ofi
module load envvar/1.0

module load PrgEnv-intel/8.2.0
module load intel/19.1.3.304
#module load netcdf/4.7.4
#module load prod_envir/2.0.6
module load prod_util/2.0.13
#module load libjpeg/9c
#module load grib_util/1.2.4
module load wgrib2/2.0.8
#for sidfex
module load python/3.8.6

# -- to check on a module's usage: module spider $m 
# Show what happened:
module list
which $WGRIB2
echo zzz `which $WGRIB2`

#From the sms.fake:
#export HOMEpmb=/gpfs/tp2/nco/ops/nwprod/util
export cyc=${cyc:-00}
export envir=developer
export code_ver=v4.0.3
export job=seaice_drift
export SMSBIN=$HOME/rgdev/${job}.${code_ver}/sms/

cd $HOME/rgdev/drift/sms/

#set -xe
set -x
tagm=20220820
tag=20220821
end=`date +"%Y%m%d" `
#end=$tag
#end=20220828

export cyc=00

while [ $tag -le $end ]
do
  export PDY=$tag
  export PDYm1=$tagm

  if [ ! -d $HOME/noscrub/com/mmab/developer/seaice_drift.${tag}${cyc} ] ; then
    #Now call J job, which will call the ex
    #export KEEPDATA="YES"
    export KEEPDATA="NO"
    time $HOME/rgdev/drift/jobs/JSEAICE_DRIFT.hind > sms.${tag}${cyc}
  fi

  tagm=$tag
  tag=`expr $tag + 1`
  tag=`$HOME/bin/dtgfix3 $tag`
done
