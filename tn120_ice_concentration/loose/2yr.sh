#!/bin/bash --login
#BSUB -J l2_2019c
#BSUB -W 5:58
#BSUB -q "dev_shared"
#BSUB -P RTO-T2O
#BSUB -R "affinity[core(1)]"
#BSUB -R "rusage[mem=8192]"
#BSUB -o l2ice_old%J.out
#BSUB -e l2ice_old%J.err
# #BSUB -N 1
# #BSUB --mail-type END,FAIL
# #BSUB --mail-user=robert.grumbine@noaa.gov

#Get a day's SSMIS L1b information and write out with concentration in NetCDF L2
#Robert Grumbine

. $MODULESHOME/init/bash
module purge
module use -a $MODULESHOME/modulefiles

module load EnvVars/1.0.3
module load ips/18.0.1.163 
module load impi/18.0.1
module load lsf/10.1

module load bufr_dumplist/2.0.0
module load dumpjb/5.0.0
module load grib_util/1.1.0
module load NetCDF/4.5.0

module load w3emc/2.3.0
module load w3nco/2.0.6

module list

set -x
export PDY=`date +"%Y%m%d"`
export HH=00
export PM=12


export EXDIR=/u/Robert.Grumbine/para/concentration/sorc/l1b_to_l2
export OUTDIR=/u/Robert.Grumbine/noscrub/2yr/
export DCOMROOT=/u/Robert.Grumbine/noscrub/2yr/

export TMPDIR=/gpfs/dell2/ptmp/wx21rg/$$
if [ ! -d $TMPDIR ] ; then
  mkdir -p $TMPDIR
  x=$?
  if [ $x -ne 0 ] ; then
    echo failed to make directory $TMPDIR, error code $x
    exit 1
  fi
fi
cd $TMPDIR
if [ `pwd` != $TMPDIR ] ; then
  echo in `pwd` rather than $TMPDIR, exiting
  exit 1
fi

set -x
export PDY=20190101

while [ $PDY -le 20191008 ]
do

  time ${EXDIR}/getday_ssmis.sh

  PDY=`expr $PDY + 1`
  PDY=`/u/Robert.Grumbine/bin/dtgfix3 $PDY`
done
