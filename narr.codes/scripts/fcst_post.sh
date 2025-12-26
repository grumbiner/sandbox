#! /bin/sh

# @ step_name = fcst_post
# @ output = job.$(jobid).$(stepid).$(step_name).out
# @ error = job.$(jobid).$(stepid).$(step_name).err
# @ job_type = parallel
# @ total_tasks = 30
#
#### @ node = 8
#### @ node_usage = not_shared
#
# @ blocking=unlimited
#
# @ wall_clock_limit = 01:30:00
# @ class = dev
## @ group = devonprod
# @ network.MPI=csss,shared,us
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

DATE=$FIRSTDATE
################################################################################
while [ $DATE -le $LASTDATE ]; do
################################################################################

YEAR=`expr substr $DATE 1 4`

export RUN_FCST=${TMPNARR}/run_${YEAR}/fcst_${DATE}
export RUN_FCST_POST=${TMPNARR}/run_${YEAR}/fcst_post_${DATE}
mkdir -p $RUN_FCST_POST
cd $RUN_FCST_POST

echo $DATE > curdate
echo "$STREAM $DATE fcst_post   `date`"> ${INIDIR}/curdate_fcst_post
. ${NARR}/scr/narr_setdate.sh

export HOLDOUT=${TMPNARR}/output_${YEAR}/fcst_${DATE}/
export COMSP=${TMPNARR}/output_${YEAR}/fcst_${DATE}/
mkdir -p ${HOLDOUT}

cp $UCL/cntrl.parm                cntrl.parm
cp $FIX32/wgt_3245_221            wgts1
## cp $FIX32/wgt_3245_221_nneighbor  wgts1

####cp $UCL/master_nn.ctl.10m_bl      master.ctl
cp $UCL/master_nn.ctl             master.ctl

#-------------------------------------------------------------------------------
for tmmark in tm00; do
#-------------------------------------------------------------------------------

export tmmark

# run post processor

cp $UCL/fcstdata.parm.${tmmark}_etag fcstdata.parm

#------------------------------------
#./EXEC_ETAPOST
FHR="00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72"
for fhr in $FHR; do

#---
ic=1
while [ $ic -lt 800 ] ; do
   if [ -s $RUN_FCST/fcstdone${fhr}.${tmmark} ]; then
     echo "file fcstdone${fhr}.${tmmark} exists" 
     break
   else
     let "ic=ic+1"
     echo "sleep 10; wait on fcstdone${fhr}.${tmmark}"
     sleep 10
   fi
   if [ $ic -ge 700 ]; then
     echo "NO fcstdone${fhr}.${tmmark} FILE after 700 iterations"
     echo "NO fcstdone${fhr}.${tmmark} FILE after 700 iterations"
     echo "NO fcstdone${fhr}.${tmmark} FILE after 700 iterations"
     exit 1
   fi
done
#---

rm -f fort*
ln -s -f fcstdata.parm                           fort.11
ln -s -f ${INPUTNARR}/nhb/nhb3245                fort.12
ln -s -f ${RUN_FCST}/restrt${fhr}.${tmmark}      fort.13
ln -s -f ${RUN_FCST}/restrt${fhr}.${tmmark}      restrt${fhr}.${tmmark}
ln -s -f cntrl.parm                              fort.14
export pgm="`basename $EXEC/etapost.x .x`"
poe $EXEC/etapost.x < $UCL/edspost.${fhr}.parm > outmeso.out${fhr}.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME

cat >input${fhr}.prd <<EOF5
$HOLDOUT/EGDAWP${fhr}.${tmmark}
EOF5

rm -f fort*
ln -s -f master.ctl   fort.10
ln -s -f wgts1        fort.21
export pgm="`basename $EXEC/prdgen.x .x`"
$EXEC/prdgen.x < input${fhr}.prd > prdgen.out${fhr}.$tmmark
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME

mv $HOLDOUT/eta.AWIP32 $HOLDOUT/AWIP32${fhr}.${tmmark}

done

#end of EXEC_ETAPOST
#------------------------------------

cp $UCL/bcexdata.parm.${tmmark} bcexdata.parm

#------------------------------------
#./EXEC_POST0

rm -f fort.*
ln -sf ${INPUTNARR}/nhb/nhb3245  fort.12
ln -sf bcexdata.parm             fort.17
ln -sf $FIX32/profdat            fort.19
#ln -sf $RUN_FCST/bcnest         fort.61
ln -sf profil.c1.${tmmark}       fort.76
export pgm=`basename $EXEC/post0.x .x`
$EXEC/post0.x < $UCL/inval.${tmmark}.post > post0.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME

#end of EXEC_POST0
#------------------------------------

#------------------------------------
#./EXEC_SNDP

rm -f fort.*
ln -s -f $UCL/sndp.parm.mono    fort.11
#ln -s -f $UCL/class0.tbl       fort.31
ln -s -f $UCL/class1.tbl        fort.32
ln -s -f profil.c1.${tmmark}    fort.66
ln -s -f class1.bufr            fort.78

export pgm=`basename $EXEC/sndp.x .x`
$EXEC/sndp.x < $UCL/modtop.parm > sndp.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME

mv class1.bufr $HOLDOUT/class1.bufr.${tmmark}

#end of EXEC_SNDP
#------------------------------------

#-------------------------------------------------------------------------------
done  ######### for tmmark in tm12 tm09 tm06 tm03; do
#-------------------------------------------------------------------------------

for fhr in $FHR; do

$EXEC/fix_rr1 ${HOLDOUT}/AWIP32${fhr}.tm00 ${HOLDOUT}/AWIP32${fhr}.${DATE} $FIX32/awipland.grb

rm ${HOLDOUT}/AWIP32${fhr}.tm00

mv ${HOLDOUT}/EGDAWP${fhr}.tm00 ${HOLDOUT}/EGDAWP${fhr}.${DATE}

done

mv ${HOLDOUT}/class1.bufr.tm00 ${HOLDOUT}/class1.bufr.${DATE}

rm -rf ${RUN_FCST}

#
# clean run_fcst_post directory
#
rm -f profil.c1.*
rm -f fort.*
rm -f bcexdata.parm cntrl.parm fcstdata.parm
rm -f curdate datetm03 datetm06 datetm09 datetm12 
rm -f master.ctl restrt??.tm??
rm -f wgts1

#==============================================================================
# transfer to HPSS
#==============================================================================
sh ${NARR}/scr/run_ftp_fcst.sh ${STREAM} ${DATE}
#==============================================================================

echo "DONE" > eta_fcst_post.$DATE

### Get ready for next cycle
DATE=`$UTIL/ndate +60 $DATE`

LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE_fcst`

################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################

echo "$0 ENDDATE "`date`

exit 0
