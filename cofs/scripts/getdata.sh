#! /bin/sh
 set -x
#
CFSWWWHOME=$HOME/WWW_cfspom
!GRIBREMOTEDIR=cfspom@asp:/com/cofs/prod
GRIBREMOTEDIR=cfspom@bsp:/com/cofs/prod
DAILYGRIB=/home/ftp/pub/cofs
#
# Tools location
#
NDATE=$HOME/ndate/ndate
#
# Date/time management
#
YYYYMMDDHHMM=`date '+%Y%m%d%H%M'`
#######YYYYMMDDHHMM=200111011324
DATE0=`echo $YYYYMMDDHHMM | cut -c1-8`'00'
echo DATE0=$DATE0
YMD=`echo $DATE0 | cut -c1-8`
YYMD=`echo -24 $DATE0 | $NDATE | cut -c1-8`
#
# Today's data directories
#
TODAYGRIB=$DAILYGRIB/$YMD
mkdir $TODAYGRIB
#
# Copy today's grib files and update Output.html
#
GRIBREMOTEDIR=$GRIBREMOTEDIR/cofs.$YMD
rcp -p $GRIBREMOTEDIR/*igb* $GRIBREMOTEDIR/*agb* \
      $GRIBREMOTEDIR/*sgb $TODAYGRIB
rm -Rf $DAILYGRIB/$YYMD
