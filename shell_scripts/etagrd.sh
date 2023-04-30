set -xeu
###
##### script /nfsuser/g01/wx21vg/cray/region/etamod/etagrd221.sh
###
WORKHOME=/nfsuser/g01/wx21vg/cray/region/etamod
WORKTMP=/gpfstmp/wx21vg/etagrd221.sh
GRBINDX=/nwprod/util/exec
##
mkdir -p $WORKTMP
cd $WORKTMP
#
export XLFRTEOPTS="unit_vars=yes"
##
##
CYCLE='t00z'
hh='00'
####     get today's date
CDATE=`cut -c7-16 /com/date/${CYCLE}`
##CDATE=2000040400
echo $CDATE > scandat1
TDATE=`cat scandat1 | awk '{ print substr($1,1,8) }'`
##
NDATEX=/nwprod/util/exec/ndate
TDATEM1=`$NDATEX -24 $CDATE | cut -c1-8`
TDATEM2=`$NDATEX -48 $CDATE | cut -c1-8`
###echo $SCOMD
#
###    grid-221   eta32 grib(349,277) gridded files
#
cp /com/eta/prod/eta.${TDATE}/eta.t00z.awip3200.tm00    eta00
cp /com/eta/prod/eta.${TDATE}/eta.t00z.awip3212.tm00    eta12
cp /com/eta/prod/eta.${TDATEM1}/eta.t00z.awip3224.tm00  eta24
cp /com/eta/prod/eta.${TDATEM1}/eta.t00z.awip3236.tm00  eta36
cp /com/eta/prod/eta.${TDATEM2}/eta.t00z.awip3248.tm00  eta48
cp /com/eta/prod/eta.${TDATEM2}/eta.t00z.awip3260.tm00  eta60
##
#
##             ETA32 index files
#
$GRBINDX/grbindex eta00  eta00.indx
$GRBINDX/grbindex eta12  eta12.indx
$GRBINDX/grbindex eta24  eta24.indx
$GRBINDX/grbindex eta36  eta36.indx
$GRBINDX/grbindex eta48  eta48.indx
$GRBINDX/grbindex eta60  eta60.indx
#
######     ETA MODEL OUTPUT   ######
#
ln -sf $WORKTMP/scandat1 fort.1
##         model output
 export XLFUNIT_20=eta00 
 export XLFUNIT_21=eta12
 export XLFUNIT_22=eta24
 export XLFUNIT_23=eta36
 export XLFUNIT_24=eta48
 export XLFUNIT_25=eta60
###
 export XLFUNIT_30=eta00.indx 
 export XLFUNIT_31=eta12.indx
 export XLFUNIT_32=eta24.indx
 export XLFUNIT_33=eta36.indx
 export XLFUNIT_34=eta48.indx
 export XLFUNIT_35=eta60.indx
#
##
timex $WORKHOME/etagrd221.x 1>ft06 2>err
##
