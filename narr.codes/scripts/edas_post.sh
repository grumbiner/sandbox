#! /bin/sh

# @ step_name = edas_post
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
# @ wall_clock_limit = 00:30:00
# @ class = dev
### @ group = devonprod
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

FIRSTDATE=`cut -c 1-10 ${INIDIR}/FIRSTDATE`
LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE`

DATE=$FIRSTDATE
################################################################################
while [ $DATE -le $LASTDATE ]; do
################################################################################

YEAR=`expr substr $DATE 1 4`

export RUN_EDAS=${TMPNARR}/run_${YEAR}/edas_${DATE}
export RUN_EDAS_POST=${TMPNARR}/run_${YEAR}/edas_post_${DATE}
mkdir -p $RUN_EDAS_POST
cd $RUN_EDAS_POST

echo $DATE > curdate
echo "$STREAM $DATE edas_post   `date`"> ${INIDIR}/curdate_edas_post
. ${NARR}/scr/narr_setdate.sh

export HOLDOUT=${TMPNARR}/output_${YEAR}/edas_${DATE}/
export COMSP=${TMPNARR}/output_${YEAR}/edas_${DATE}/
mkdir -p ${HOLDOUT}

cp $UCL/cntrl.parm                cntrl.parm
cp $FIX32/wgt_3245_221            wgts1
## cp $FIX32/wgt_3245_221_nneighbor  wgts1

cp $UCL/master_nn.ctl             master.ctl

#-------------------------------------------------------------------------------
for tmmark in tm12 tm09 tm06 tm03; do
#-------------------------------------------------------------------------------

export tmmark

# run post processor

cp $UCL/fcstdata.parm.${tmmark}_etag fcstdata.parm

#------------------------------------
#./EXEC_ETAPOST
FHR="00 03"
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
#ln -sf $RUN_EDAS/bcnest         fort.61
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

$EXEC/fix_rr1 ${HOLDOUT}/AWIP32${fhr}.tm12 ${HOLDOUT}/AWIP32${fhr}.${DATM12} $FIX32/awipland.grb
$EXEC/fix_rr1 ${HOLDOUT}/AWIP32${fhr}.tm09 ${HOLDOUT}/AWIP32${fhr}.${DATM09} $FIX32/awipland.grb
$EXEC/fix_rr1 ${HOLDOUT}/AWIP32${fhr}.tm06 ${HOLDOUT}/AWIP32${fhr}.${DATM06} $FIX32/awipland.grb
$EXEC/fix_rr1 ${HOLDOUT}/AWIP32${fhr}.tm03 ${HOLDOUT}/AWIP32${fhr}.${DATM03} $FIX32/awipland.grb

sh /u/wx51we/home/rr/bin/rot_vector.sh ${HOLDOUT}/AWIP32${fhr}.${DATM12} ${HOLDOUT}/AWIP32${fhr}.${DATM12}.tmp
mv ${HOLDOUT}/AWIP32${fhr}.${DATM12}.tmp ${HOLDOUT}/AWIP32${fhr}.${DATM12}

sh /u/wx51we/home/rr/bin/rot_vector.sh ${HOLDOUT}/AWIP32${fhr}.${DATM09} ${HOLDOUT}/AWIP32${fhr}.${DATM09}.tmp
mv ${HOLDOUT}/AWIP32${fhr}.${DATM09}.tmp ${HOLDOUT}/AWIP32${fhr}.${DATM09}

sh /u/wx51we/home/rr/bin/rot_vector.sh ${HOLDOUT}/AWIP32${fhr}.${DATM06} ${HOLDOUT}/AWIP32${fhr}.${DATM06}.tmp
mv ${HOLDOUT}/AWIP32${fhr}.${DATM06}.tmp ${HOLDOUT}/AWIP32${fhr}.${DATM06}

sh /u/wx51we/home/rr/bin/rot_vector.sh ${HOLDOUT}/AWIP32${fhr}.${DATM03} ${HOLDOUT}/AWIP32${fhr}.${DATM03}.tmp
mv ${HOLDOUT}/AWIP32${fhr}.${DATM03}.tmp ${HOLDOUT}/AWIP32${fhr}.${DATM03}

rm ${HOLDOUT}/AWIP32${fhr}.tm12
rm ${HOLDOUT}/AWIP32${fhr}.tm09
rm ${HOLDOUT}/AWIP32${fhr}.tm06
rm ${HOLDOUT}/AWIP32${fhr}.tm03

mv ${HOLDOUT}/EGDAWP${fhr}.tm12 ${HOLDOUT}/EGDAWP${fhr}.${DATM12}
mv ${HOLDOUT}/EGDAWP${fhr}.tm09 ${HOLDOUT}/EGDAWP${fhr}.${DATM09}
mv ${HOLDOUT}/EGDAWP${fhr}.tm06 ${HOLDOUT}/EGDAWP${fhr}.${DATM06}
mv ${HOLDOUT}/EGDAWP${fhr}.tm03 ${HOLDOUT}/EGDAWP${fhr}.${DATM03}

done

mv ${HOLDOUT}/class1.bufr.tm12 ${HOLDOUT}/class1.bufr.${DATM12}
mv ${HOLDOUT}/class1.bufr.tm09 ${HOLDOUT}/class1.bufr.${DATM09}
mv ${HOLDOUT}/class1.bufr.tm06 ${HOLDOUT}/class1.bufr.${DATM06}
mv ${HOLDOUT}/class1.bufr.tm03 ${HOLDOUT}/class1.bufr.${DATM03}

mv ${RUN_EDAS}/prepout.${DATM12} ${HOLDOUT}/.
mv ${RUN_EDAS}/prepout.${DATM09} ${HOLDOUT}/.
mv ${RUN_EDAS}/prepout.${DATM06} ${HOLDOUT}/.
mv ${RUN_EDAS}/prepout.${DATM03} ${HOLDOUT}/.

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

LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE`

################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################

echo "$0 ENDDATE "`date`

exit 0
