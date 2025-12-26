#!/bin/sh

# @ step_name = r2_model
# @ output = job.$(jobid).$(stepid).$(step_name).out
# @ error = job.$(jobid).$(stepid).$(step_name).err
# @ job_type = serial
# @ wall_clock_limit = 01:00:00
# @ class = dev
## @ group = devonprod
# @ queue

set -aeux

export PS4='$SECONDS + '
echo "$0 STRDATE "`date`

echo "=====ENV====="
env
if [ -z "${LOADL_JOB_NAME:-}" ]; then
  LOADL_JOB_NAME=$$
else
  echo ${LOADL_JOB_NAME} > LLJOB_ID_${LOADL_STEP_NAME}
fi
echo "=====ENV====="

echo ${NARR:?"NARR is not set; fatal error"}

export STREAM=`cut -c 1-2 STREAM`
readonly STREAM
echo ${STREAM:?"STREAM is not set; fatal error"}

. ${NARR}/scr/narr_envir.sh

INIDIR=`pwd`
readonly INIDIR

FIRSTDATE=`cut -c 1-10 ${INIDIR}/FIRSTDATE_fcst`
LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE_fcst`

#
# run R2 model
#
# requires arguement: date (YYYYMMDDHH)
#

#if [ $# -ne 0 ] ; then
#  date=$1
#fi
#
#if [ "$date" = "" ] ; then
#  echo "no date code"
#  exit 8
#fi


DATE=$FIRSTDATE
################################################################################
################################################################################
while [ $DATE -le $LASTDATE ]; do
################################################################################
################################################################################

YEAR=`expr substr $DATE 1 4`

date=$DATE

#
# dir=location to save calculations
# bcdir=location of sigma and sfc analyses
# tdir=location to do calculations: fast and big disk
# utils=location of utilites
#

dir=${NARR}/r2_fcst/r2_${YEAR}/${DATE}/
mkdir -p $dir

bcdir_sig=${NARR}/input/r2_sigma/${YEAR}
bcdir_sfc=${NARR}/input/r2_sfcanl/${YEAR}

tdir=/gpfstmp/$LOGNAME/r2.$date.short
utils=/u/wx51we/home/r1/utils
export dir bcdir tdir utils


sfcanl=$bcdir_sfc/sfc.anl.$date.ieee
sanl=$bcdir_sig/sig.anl.$date.ieee
if [ ! -f $sanl -o ! -f $sfcanl ] ; then
  echo "missing initial conditions"
  exit 8
fi

mkdir -p $tdir
[ "$?" -ne 0 ] && exit 7

# mrf=r2 executable
# fixed  location of fixed fields

mrf=/u/wx51we/home/r2/bin/r2noDG3.x
fixed=/u/wx51we/home/r2/fixed

#
# start saving data
# end of run at endhour
# dhr = output interval

endhour=72
dhr=6

# save sanl file
cp $sanl $dir/sig.f00.$date

cd $tdir
[ $? -ne 0 ] && exit 8

# convert sanl sfcanl to double precision

$utils/sig2dbl $sanl sigft.00
$utils/sig2dbl $sfcanl sfcft.00
ln -sf sigft.00 sigdt.00

date

# kana script
DELTAT=1800
SHORT_WAVE_INTVL=1
LONG_WAVE_INTVL=1

HDIFF_Q_T_RATIO=1.0

inihr_gbl=0
swhr_gbl=${SHORT_WAVE_INTVL:=3}
lwhr_gbl=${LONG_WAVE_INTVL:=3}
#

echo " &namsmf NUM(5)=0," >parm
echo " CON(1)=${DELTAT:=2400}.,CON(3)=$inihr_gbl., " >>parm
echo " CON(4)=$swhr_gbl.,CON(5)=$lwhr_gbl.," >>parm
echo " CON(7)=${INCHOUR:=$dhr}., " >>parm
echo " CON(9)=${PRTHOUR:=$dhr}.,CON(17)=${endhour:=$dhr}., " >>parm
echo " CON(8)=$HDIFF_Q_T_RATIO," >>parm
echo " NUM(1300)=0," >>parm
echo " / " >>parm

# ulimit -s 90000
ulimit -s  120000
# ulimit -s  40000

hr=0

while [ $hr -lt $endhour ]
do
   nexthr=`expr $hr + $dhr`
   [ $hr -le 9 ] && hr=0$hr
   [ $nexthr -le 9 ] && nexthr=0$nexthr

   touch fort.0
   rm fort.*

   ln -fs sigft.$hr			fort.11
   ln -sf sigdt.$hr			fort.12
   ln -sf sfcft.$hr			fort.14

   ln -fs $fixed/co2con.dbl		fort.15
   ln -fs heatrate			fort.21
   ln -fs $fixed/mtnvar.dbl		fort.24
   ln -sf $fixed/tune_new_max_nvvl.197901-12.ewmrge.sngl.dbl fort.43

   ln -fs $fixed/gcmo3.asc		fort.48
   ln -fs $fixed/albaer.snl.dbl		fort.49

   ln -fs sigft.$nexthr.a		fort.51
   ln -fs sigdt.$nexthr.a		fort.52
   ln -sf sfcft.$nexthr.a		fort.53

   # ln -sf znl$date.f00		fort.61
   ln -sf /dev/null 			fort.61

   # ln -fs flx$date.f00		fort.62
   ln -sf /dev/null			fort.62

   #ln -fs flx$date.f$nexthr	 	fort.63
   ln -fs $dir/flx$date.f$nexthr	 	fort.63

   # ln -fs znl$date.f$nexthr		fort.64
   ln -fs /dev/null			fort.64

   # ln -fs dg3$date.f00		fort.65
   ln -fs /dev/null			fort.65

   # ln -fs dg3f$date.f$nexthr		fort.66
   ln -fs /dev/null			fort.66

   time $mrf <parm >last.out
   [ $? -ne 0 ] && exit 8

   # check the file size
   s1=`wc -c <sigft.$nexthr.a`
   s2=`wc -c <sigdt.$nexthr.a`
   s3=`wc -c <sfcft.$nexthr.a`
   if [ $s1 -ne 3824520 -o $s2 -ne 3824520 -o $s3 -ne 2310328 ] ; then
      rm sigft.$nexthr.a sigdt.$nexthr.a sfcft.$nexthr.a
      echo "***bad run***"
      exit 8
   fi

   # finished processing 
   mv sfcft.$nexthr.a sfcft.$nexthr
   mv sigft.$nexthr.a sigft.$nexthr
   mv sigdt.$nexthr.a sigdt.$nexthr

   # save sanl file as single precision
   $utils/sig2sngl sigft.$nexthr $dir/sig.f$nexthr.$date
   
   # remove previous file to save space
   if [ $hr -ge 1 ] ; then
      rm sigft.$hr sigdt.$hr sfcft.$hr
   fi

   hr=`expr $hr + $dhr`
done

# cleanup $tdir

cd $tdir
[ $? -ne 0 ] && exit 8
rm *
cd $dir
rmdir $tdir

echo "DONE" > $dir/r2_model_done.$date

# Get ready for next cycle
DATE=`/nwprod/util/exec/ndate +60 $DATE`

################################################################################
################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################
################################################################################

echo "$0 ENDDATE "`date`

exit 0
