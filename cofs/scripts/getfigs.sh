!# /bin/sh
 set -x
#
WWWDIR=/data/WWW/cofs
#FIGSREMOTEDIR=cfspom@sgi100:/migr/data/cfspom/output/prod/tmp
FIGSREMOTEDIR=cofs@sgi108:/cofs/home/cofs/cfspom/output/prod
DAILYFIGS=$WWWDIR/figures
#
NDATE=$HOME/ndate/ndate
PS2GIF=/usr/local/bin/netpbm/pstogif
#
YYYYMMDDHHMM=`date '+%Y%m%d%H%M'`
DATE0=`echo $YYYYMMDDHHMM | cut -c1-8`'00'
echo DATE0=$DATE0
YMD=`echo $DATE0 | cut -c1-8`
YYMD=`echo -24 $DATE0 | $NDATE | cut -c1-8`
#
# Today's data directories
#
TODAYFIGS=$DAILYFIGS/$YMD
mkdir $TODAYFIGS
rm $TODAYFIGS/*.gif
for TVAL in 00 24 48
do
  CDATE=`echo $TVAL $DATE0 | $NDATE`
  rcp -p $FIGSREMOTEDIR/$CDATE*.ps $TODAYFIGS/.
  set x $TODAYFIGS/$CDATE*.ps
  for file
  do 
    $PS2GIF $file -page letter -rotate -90
  done
#  ls -d $TVAL* | sed `s/\(.*\)\
done
rm -Rf $DAILYFIGS/$YYMD
rm $WWWDIR/x.gif
