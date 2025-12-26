#!/bin/sh
#
# this code merges the awip analyses and 3 hour forecast
# and produces a file that contains the analyses
# fluxes (0-3hr) and the cloud/precip from the 3 hour fcst
# note: want the file to be grads compatible
#  that means no two fields have the same variable and levels
#
# 8/2003 w. ebisuzaki
#

if [ $# -ne 3 ] ; then
    echo "merges awip analysis and fcst file"
    echo " to get analyses and fluxes"
    echo " usage: $0 analyses fcst output"
    exit 8
fi

wgrib=/u/wx51we/bin/wgrib

list='(ACPCP|APCP|APCPN|BGRUN|CDCON|CDLYR|CFRZR|CICEP|CRAIN|CSNOW|DLWRF|DSWRF|EVP|GFLUX|HCDC|LCDC|LHTFL|MCDC|PEVAP|PRATE|SHTFL|SNOHF|SNOM|SSRUN|TCDC|ULWRF|USWRF|WCCONV|WCINC|WCUFLX|WCVFLX|WVCONV|WVINC|WVUFLX|WVVFLX)'

# check date codes of files
inv="`$wgrib -d 1 -s $1 -o /dev/null`"
if [ "`echo $inv | grep -c ':anl:'`" -ne 1 ] ; then
   echo "$1 is not an analyses"
   exit 8
fi
date1=`echo "$inv" | cut -f3 -d:`

inv="`$wgrib -d 1 -s $2 -o /dev/null`"
if [ "`echo $inv | grep -c ':3hr fcst:'`" -ne 1 ] ; then
   echo "$2 is not a 3 hour forecast"
   exit 8
fi
date2=`echo "$inv" | cut -f3 -d:`

if [ "$date1" != "$date2" ] ; then
   echo "dates do not match in $0"
   echo "fatal error"
   exit 8
fi

# get rid of flux variables from analyses

$wgrib -s $1 | \
  egrep -v "$list" | \
  $wgrib -s -i $1 -grib -o $3

# 
# some of the wanted variables in the flux file
# are repeated ex. "3 hr fcst" "0-3 hr average"
# need to remove repeated variables
#
# keep 3 hr fcst TCDC to remain consistent with layer clouds
#

$wgrib -s $2 | \
  egrep "$list" | \
  egrep -v ':(SHTFL|LHTFL|DSWRF|DLWRF|GFLUX|USWRF|ULWRF):sfc:3hr fcst:' | \
  egrep -v 'TCDC:atmos col:0-3hr ave' | \
  $wgrib -s -i $2 -grib -o $3 -append

dir=`dirname $3`
file=`basename $3`

ftp wesley << EOF
binary
cd /var/ftp/pub/regional_reanl/analyses/merged_AWIP32
lcd $dir
put $file
quit
EOF
