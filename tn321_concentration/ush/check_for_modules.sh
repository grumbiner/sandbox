module purge

#-----------------------------------------------
# Check for availability 
module spider envvar
module spider PrgEnv-intel
module spider intel

#NCO build libraries for grib, bufr, ...
module spider w3nco 
module spider w3emc
module spider bacio
module spider png
module spider jasper
module spider g2
module spider bufr
module spider zlib

#-----------------------------------------------
#  Try loading:
module load envvar/1.0
module load PrgEnv-intel
module load intel/19.1.3.304
module load w3nco/2.4.1
module load w3emc/2.9.2
module load bacio/2.4.1
module load libpng/1.6.37
module load jasper/2.0.25
module load g2/3.4.5
module load bufr/11.7.0
module load zlib/1.2.11
module list

which CC ftn

module load craype-x86-rome
module load libfabric/1.11.0.0
module load craype-network-ofi
