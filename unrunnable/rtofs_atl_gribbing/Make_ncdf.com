#!/bin/csh
#
#set echo
#
# --- Usage:  ./Make_ncdf.com >& Make_ncdf.log
#
# --- make all netCDF archive executables
#
# --- set NCDF to the root directory for netCDF version 3.5.
# --- available from: http://www.unidata.ucar.edu/packages/netcdf/
#
setenv NCDF /net/ajax/scrf/wallcraf/NETCDF/netcdf-3.5.0
#
# --- set ARCH to the correct value for this machine.
#
source Make_all.src
#
echo "NCDF = " $NCDF
echo "ARCH = " $ARCH
#
if (! -e ../../config/${ARCH}_setup) then
  echo "ARCH = " $ARCH "  is not supported"
  exit 1
endif
#
# --- softlink to netCDF module and library
#
/bin/rm -f netcdf.mod libnetcdf.a
#
ln -s ${NCDF}/include/netcdf.mod .
ln -s ${NCDF}/lib/libnetcdf.a    .
#
# --- netCDF archive programs
#
foreach m ( archv2ncdf2d archm2ncdf2d archv2ncdf3z archm2ncdf3z)
  make ${m} ARCH=${ARCH} >&! Make_${m}
  if ($status) then
    echo "Make failed:" ${m}
  else
    echo "Make worked:" ${m}
  endif
end
