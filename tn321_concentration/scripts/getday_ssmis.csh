#!/bin/csh -fx

#Get a day's SSMIS L1b information and write out with concentration in NetCDF L2
#Robert Grumbine

if ( ! -f ${OUTDIR}/l2.f285.51.${PDY}.nc || ! -f ${OUTDIR}/l2.f286.52.${PDY}.nc ) then
  #Get the day's L1b ssmi-s data
  #  HH is the central hour (UTC)
  #  PM is hours plus or minus -- i.e., window half-width, 12 to get a day's data
  #  ssmisu specifies the SSMI-S instruments (all of them)
  #  ca. 50 seconds on phase 1
  echo about to try: ksh ${DUMPJB} ${PDY}${HH} ${PM} ssmisu 
  time ksh ${DUMPJB} ${PDY}${HH} ${PM} ssmisu 
    
  #Main work of L1b to L2 netcdf -- ca. 500 seconds on phase 1
  #Main work of L1b to L2 netcdf -- ca. 360 seconds on phase 3
  #Main work of L1b to L2 netcdf -- ca. 300 seconds on theia
  #  n.b.: Most time is spent in decoding the bufr data
  ln -sf ssmisu.ibm fort.11 #input data must be in fortran unit 11
  time ${EXDIR}/ssmis_tol2_both > out
  mv l2out.f285.51.nc ${OUTDIR}/l2.f285.51.${PDY}.nc
  mv l2out.f286.52.nc ${OUTDIR}/l2.f286.52.${PDY}.nc

else
  echo already have files for $PDY

endif
