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

export PDY=${PDY:-20220111}
export PDYm1=${PDYm1:-20220110}

export COMOUT=$COMROOT/seaice/developer/amsr2.$PDY
if [ ! -d $COMOUT ] ; then
  mkdir -p $COMOUT
fi

#zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
#getamsr (bufr) (to noscrub/seaice/developer/amsr2.$PDY

time $DUMPJB ${PDY}00 12 amsr2

#copy+rename ice conc from $HOME/noscrub/sice/sice.$PDY/  to same output dir
cp -p $HOME/noscrub/sice/sice.$PDY/seaice.t00z.fill5min $COMOUT/ice.$PDY
# avhrr
mv $HOME/noscrub/sice/sice.$PDY/land.$PDYm1 $COMOUT
mv $HOME/noscrub/sice/sice.$PDY/seas.$PDYm1 $COMOUT

#bufr to binary
ln -sf amsr2.ibm fort.11
time $HOME/rgdev/new_concentrations/amsr2/exec/seaice_amsrbufr
mv fort.52 amsr2.$PDY
cp  amsr2.$PDY $COMOUT

time $HOME/rgdev/new_concentrations/amsr2/sorc/amsr2_l1b_scan $COMOUT/amsr2.$PDY hr lr $COMOUT/ice.$PDY > txt.$PDY
grep lr txt.$PDY > lr.txt.$PDY

time python3 $HOME/rgdev/new_concentrations/amsr2/sorc/amsr2.filter/amsr2g.py lr.txt.$PDY > out.$PDY
mv lr.txt.$PDY out.$PDY $COMOUT
mv perfect0 $COMOUT/perfect0.$PDY
mv postperfect $COMOUT/postperfect.$PDY

cd ..
rm -rf $DATA
