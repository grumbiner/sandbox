#!/bin/csh
#
#set echo
#
# --- Usage:  ./Make_all.com >& Make_all.log
#
# --- make all archive executables (except netCDF)
#
# --- set ARCH to the correct value for this machine.
#
source Make_all.src
#
printenv ARCH
#
if (! -e ../../config/${ARCH}_setup) then
  echo "ARCH = " $ARCH "  is not supported"
  exit 1
endif
#
# --- archive modifying programs
#
foreach m ( hycomarchv micomarchv mrgl_archv trim_archv archv2data2d archm2data2d archv2data3z archm2data3z archv2restart archm2restart )
  make ${m} ARCH=${ARCH} >&! Make_${m}
  if ($status) then
    echo "Make failed:" ${m}
  else
    echo "Make worked:" ${m}
  endif
end
