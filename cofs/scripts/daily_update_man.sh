#! /bin/sh
 set -x
#
CFSWWWHOME=$HOME/WWW_cfspom
WWWDIR=/data/WWW/cofs
FIGSREMOTEDIR=cfspom@prodsp:/gpfstmp/cfspom/PROD_OUT
GRIBREMOTEDIR=cfspom@prodsp:/com/cofs/prod
DAILYFIGS=$WWWDIR/figures
DAILYGRIB=/var/ftp/pub/cofs
#
# Tools location
#
NDATE=$HOME/ndate/ndate
SCP=/usr/local/bin/scp
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
TODAYFIGS=$DAILYFIGS/$YMD
TODAYGRIB=$DAILYGRIB/$YMD
mkdir $TODAYFIGS $TODAYGRIB
rm $TODAYFIGS/*.gif
#
# Create temporary HTML files
#
cp $CFSWWWHOME/Welcome_tplate.html $CFSWWWHOME/tmp/Welcome_tmp.html
cp $CFSWWWHOME/Output_tplate.html $CFSWWWHOME/tmp/Output_tmp.html
#
# Copy today's grib files and update Output.html
#
GRIBREMOTEDIR=$GRIBREMOTEDIR/cofs.$YMD
   $SCP -p $GRIBREMOTEDIR/*igb* $GRIBREMOTEDIR/*agb* \
      $GRIBREMOTEDIR/*sgb $TODAYGRIB
`sed s/YYYYMMDDGRIB/$YMD/g $CFSWWWHOME/tmp/Output_tmp.html > $WWWDIR/Output.html`
#
# Get figures from remote host and update Welcome.html
#
cd $WWWDIR
for TVAL in 00 24 48
do
  CDATE=`echo $TVAL $DATE0 | $NDATE`
  echo 'TVAL='$TVAL ', CDATE'=$CDATE
  $SCP -p $FIGSREMOTEDIR/$CDATE*.gif $TODAYFIGS/.
  `sed s/YYYYMMDD$TVAL/$CDATE/g $CFSWWWHOME/tmp/Welcome_tmp.html > $CFSWWWHOME/tmp/asdf`
  mv -f $CFSWWWHOME/tmp/asdf  $CFSWWWHOME/tmp/Welcome_tmp.html
done
`sed s/YYYYMMDDFIG/$YMD/g $CFSWWWHOME/tmp/Welcome_tmp.html > $WWWDIR/Welcome.html`
#
# Remove old and temporary data data
#
rm -Rf $DAILYGRIB/$YYMD
rm -Rf $DAILYFIGS/$YYMD
rm $WWWDIR/x.gif
chmod u+w $CFSWWWHOME/tmp/Output_tmp.html $CFSWWWHOME/tmp/Welcome_tmp.html
rm $CFSWWWHOME/tmp/Output_tmp.html $CFSWWWHOME/tmp/Welcome_tmp.html
