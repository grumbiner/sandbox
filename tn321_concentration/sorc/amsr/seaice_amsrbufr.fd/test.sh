#! /bin/sh
#set -x
#
SORC=/marine/save/wx21vg/amsre/btmps
WORKDIR=/stmp/wx21vg/test.sh
DATADIR=/dcom/us007003
############################
##
mkdir -p $WORKDIR
##
cd $WORKDIR
export XLFRTEOPTS="unit_vars=yes"
##
# Date Manipulation here
#
HR="00"
cp /com/date/t${HR}z  date
CDATE=`cut -c7-14 date`
echo CDATE $CDATE
##
#
CDATE=20090406
################################################
#          STEP    1.
# dump sst datA
################################################
#
#
  cp $DATADIR/$CDATE/b021/xx254 amsre_btmps 
    
    export XLFUNIT_11="amsre_btmps"
    export XLFUNIT_51="output.$CDATE"
#
    timex $SORC/test.x > ft06 2>>err
#
rm amsre_btmps
