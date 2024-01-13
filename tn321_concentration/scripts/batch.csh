#!/bin/csh -fx
#SBATCH -J l2ice_2011q2
#SBATCH -t 5:57:00
#SBATCH -q batch
#SBATCH -A marine-cpu
#SBATCH --ntasks 1
#SBATCH -e slurm%j.out
#SBATCH -o slurm%j.out
#SBATCH --mail-type FAIL
#SBATCH --mail-user=robert.grumbine@noaa.gov

#Get a day's SSMIS L1b information and write out with concentration in NetCDF L2
#Robert Grumbine
source /etc/profile.d/modules.csh
setenv EXDIR  /home/Robert.Grumbine/save/concentration.gerrit/sorc/l1b_to_l2

module purge
module use /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles/

module load intel/18.0.5.274
module load impi/2018.0.4

module load wgrib2/2.0.8
module load grib_util/1.1.1
module load netcdf/4.6.1
module load w3lib/2.0.2
#module load prod_util/1.1.0
module load dumpjb 
module load bufr_dumplist
#Need these if/when don't have dumpjb, bufr_dumplist modules, as on hera
#setenv DUMPJB $EXDIR/modules/obsproc_dump.v5.0.2/ush/dumpjb
#setenv HOMEobsproc_dump $EXDIR/modules/obsproc_dump.v5.0.2
#setenv HOMEobsproc_shared_bufr_dumplist $EXDIR/modules/bufr_dumplist.v2.0.2
#module use $EXDIR/modules/obsproc_dump.v5.0.2/
#module load modulefiles/dumpjb/5.0.2
#
#module use $EXDIR/modules/bufr_dumplist.v2.0.2/modulefiles
#module load bufr_dumplist
#
module list
#exit

setenv PDY `date +"%Y%m%d"`
setenv HH 00
setenv PM 12

setenv OUTDIR /scratch2/NCEPDEV/marine/marineda/ocean_observations/data/icec_l2.emc
setenv DCOMROOT /home/Robert.Grumbine/save/concentration.gerrit/sorc/l1b_to_l2/raw_ssmis/

setenv TMPDIR  ${OUTDIR}/scratch2/Robert.Grumbine/$$

if ( ! -d $TMPDIR ) then
  mkdir -p $TMPDIR
  setenv x $status
  if ( $x != 0 ) then
    echo failed to make directory $TMPDIR, error code $x
    exit 1
  endif
endif
cd $TMPDIR
if ( `pwd` != $TMPDIR ) then
  echo in `pwd` rather than $TMPDIR, exiting
  exit 1
endif

setenv PDY 20110101
env >& environment

while ( $PDY <= 20111231 )

  echo hello from main, about to call getday $PDY
  time ${EXDIR}/getday_ssmis.csh
  echo hello from main, back from getday
  rm *

  setenv PDY `expr $PDY + 1`
  setenv PDY `/home/Robert.Grumbine/bin/dtgfix3 $PDY`
end
