#!/bin/csh -f
#SBATCH --ntasks=8
#SBATCH -t 10
#SBATCH -A marine-cpu
#SBATCH -J shallow
#SBATCH -q batch
# #PBS -e /home/Robert.Grumbine/shallow/shallow.out
# #PBS -o /home/Robert.Grumbine/shallow/shallow.out
#SBATCH -D .


#module purge
#module load intel/17.0.5.238
#module load impi
module list
echo hello from slurm

set echo
setenv OS `uname`
setenv NOMP 0
setenv NMPI 8

setenv S /scratch3/NCEPDEV/marine/shallow/test
setenv HEXE /home/Robert.Grumbine/shallow/shallow
mkdir -p $S
cd       $S

    /bin/rm -f core
    touch core
    #time mpirun -np $NMPI ${HEXE}
    time srun  ${HEXE}
