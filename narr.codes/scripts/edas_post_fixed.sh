#! /bin/sh

# @ step_name = edas_post_fixed
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
#### @ wall_clock_limit = 00:00:00
# @ class = dev
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

FIRSTDATE=1988080100
LASTDATE=$FIRSTDATE

DATE=$FIRSTDATE
################################################################################
while [ $DATE -le $LASTDATE ]; do
################################################################################

YEAR=`expr substr $DATE 1 4`

export RUN_EDAS=${TMPNARR}/run_${YEAR}/edas_${DATE}
export RUN_EDAS_POST=${TMPNARR}/run_${YEAR}/edas_post_fixed_${DATE}
mkdir -p $RUN_EDAS_POST
cd $RUN_EDAS_POST

echo $DATE > curdate
. ${NARR}/scr/narr_setdate.sh

export HOLDOUT=${TMPNARR}/output_${YEAR}/edas_fixed_${DATE}/
export COMSP=${TMPNARR}/output_${YEAR}/edas_fixed_${DATE}/
mkdir -p ${HOLDOUT}

cp $UCL/cntrl_fixed.parm          cntrl.parm
cp $FIX32/wgt_3245_221            wgts1
## cp $FIX32/wgt_3245_221_nneighbor  wgts1

cp $UCL/master_fixed.parm         master.ctl

#-------------------------------------------------------------------------------
for tmmark in tm12; do
#-------------------------------------------------------------------------------

export tmmark

# run post processor

cp $UCL/fcstdata.parm.${tmmark}_etag fcstdata.parm

#------------------------------------
#./EXEC_ETAPOST
FHR="00"
for fhr in $FHR; do

#---
ic=1
while [ $ic -lt 600 ] ; do
   if [ -s $RUN_EDAS/fcstdone${fhr}.${tmmark} ]; then
     echo "file fcstdone${fhr}.${tmmark} exists" 
     break
   else
     let "ic=ic+1"
     echo "sleep 10; wait on fcstdone${fhr}.${tmmark}"
     sleep 10
   fi
   if [ $ic -ge 500 ]; then
     echo "NO fcstdone${fhr}.${tmmark} FILE after 500 iterations"
     echo "NO fcstdone${fhr}.${tmmark} FILE after 500 iterations"
     echo "NO fcstdone${fhr}.${tmmark} FILE after 500 iterations"
     exit 1
   fi
done
#---

rm -f fort*
ln -s -f fcstdata.parm                           fort.11
ln -s -f ${INPUTNARR}/nhb/nhb3245                fort.12
ln -s -f ${RUN_EDAS}/restrt${fhr}.${tmmark}      fort.13
ln -s -f ${RUN_EDAS}/restrt${fhr}.${tmmark}      restrt${fhr}.${tmmark}
ln -s -f cntrl.parm                              fort.14
export pgm="`basename $EXEC/etapost.x .x`"
poe $EXEC/etapost.x < $UCL/edspost.${fhr}.parm > outmeso.out${fhr}.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME

cat >input${fhr}.prd <<EOF5
$HOLDOUT/EGDFIX${fhr}.${tmmark}
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

#-------------------------------------------------------------------------------
done  ######### for tmmark in tm12 tm09 tm06 tm03; do
#-------------------------------------------------------------------------------

mv ${HOLDOUT}/AWIP3200.tm12 ${HOLDOUT}/AWIP32.fixed

mv ${HOLDOUT}/EGDFIX00.tm12 ${HOLDOUT}/EGDFIX.fixed

#
# clean run_edas_post directory
#
rm -f profil.c1.*
rm -f fort.*
rm -f bcexdata.parm cntrl.parm fcstdata.parm
rm -f curdate datetm03 datetm06 datetm09 datetm12 
rm -f master.ctl restrt??.tm??
rm -f wgts1

echo "DONE" > eta_edas_post.$DATE

### Get ready for next cycle
DATE=`$UTIL/ndate +12 $DATE`

################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################

echo "$0 ENDDATE "`date`

exit 0
