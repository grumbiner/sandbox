#! /bin/sh

# @ step_name = fcst
# @ output = job.$(jobid).$(stepid).$(step_name).out
# @ error = job.$(jobid).$(stepid).$(step_name).err
# @ job_type = parallel
# @ total_tasks = 42
#
#### @ node = 14
#### @ node_usage = not_shared
#
# @ blocking = unlimited
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

export RUN_EDAS=${TMPNARR}/run_${YEAR}/edas_restrt00.tm00.${DATE}
export RUN_FCST=${TMPNARR}/run_${YEAR}/fcst_${DATE}
mkdir -p $RUN_FCST
cd $RUN_FCST
rm -f $RUN_FCST/*

echo $DATE > curdate
echo "$STREAM $DATE fcst        `date`"> ${INIDIR}/curdate_fcst
. ${NARR}/scr/narr_setdate.sh

if [ $CYC = 12 ]; then
   cp $UCL/rstupt.parm_newsnow snow.parm
else
   cp $UCL/rstupt.parm_cycsnow snow.parm
fi

date

export tmmark=tm00
export itag=03
export restrtahr=03

#---
ic=1
while [ $ic -lt 800 ] ; do
   if [ -s $RUN_EDAS/fcstdone00.tm00 ]; then
     echo "file $RUN_EDAS/fcstdone00.tm00 exists"
     break
   else
     let "ic=ic+1"
     echo "sleep 10; wait on $RUN_EDAS/fcstdone00.tm00"
     sleep 10
   fi
   if [ $ic -ge 700 ]; then
     echo "NO $RUN_EDAS/fcstdone00.tm00 FILE after 700 iterations"
     echo "NO $RUN_EDAS/fcstdone00.tm00 FILE after 700 iterations"
     echo "NO $RUN_EDAS/fcstdone00.tm00 FILE after 700 iterations"
     exit 8
   fi
done

ic=1
while [ $ic -lt 800 ] ; do
   if [ -s ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE} ]; then
     echo "file ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE} exists"
     break
   else
     let "ic=ic+1"
     echo "sleep 10; wait on ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE}"
     sleep 10
   fi
   if [ $ic -ge 700 ]; then
     echo "NO ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE} FILE after 700 iterations"
     echo "NO ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE} FILE after 700 iterations"
     echo "NO ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE} FILE after 700 iterations"
     exit 8
   fi
done
#---

date

##### compute boundary conditions

cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f00.${DATE} avnbc0
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f06.${DATE} avnbc1
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f12.${DATE} avnbc2
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f18.${DATE} avnbc3
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f24.${DATE} avnbc4
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f30.${DATE} avnbc5
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f36.${DATE} avnbc6
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f42.${DATE} avnbc7
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f48.${DATE} avnbc8
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f54.${DATE} avnbc9
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f60.${DATE} avnbc10
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f66.${DATE} avnbc11
cp ${NARR}/r2_fcst/r2_${YEAR}/${DATE}/sig.f72.${DATE} avnbc12

export tmmark=t00s   ##### temporarily set tmmark to t00s #####

#------------------------------------
#./EXEC_BND_FCST
rm -f fort.*

ln -s -f avnbc0                    fort.11
ln -s -f avnbc1                    fort.12
ln -s -f avnbc2                    fort.13
ln -s -f avnbc3                    fort.14
ln -s -f avnbc4                    fort.15
ln -s -f avnbc5                    fort.16
ln -s -f avnbc6                    fort.17
ln -s -f avnbc7                    fort.18
ln -s -f avnbc8                    fort.19
ln -s -f avnbc9                    fort.20
ln -s -f avnbc10                   fort.21
ln -s -f avnbc11                   fort.22
ln -s -f avnbc12                   fort.23
ln -s -f nmcdate                   fort.40
ln -s -f $UCL/modtop.parm          fort.41
ln -s -f $FIX32/deta_ldt1.45.25mb  fort.42
ln -s -f rvetalbc                  fort.51
ln -s -f etabcs00                  fort.52

export pgm="`basename $EXEC/mkbnd.x .x`"
$EXEC/mkbnd.x < $UCL/mkbnd.parm_13 > mkbnd.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME

rm avnbc*
#end of EXEC_BND_FCST
#------------------------------------

cp ${RUN_EDAS}/nbc3245 nbc3245

#------------------------------------
#./EXEC_BCTEND
rm -f fort.*

ln -s -f etabcs00  fort.17
ln -s -f nbc3245   fort.18
ln -s -f nbcout    fort.51
export pgm="`basename $EXEC/bctend.x .x`"
$EXEC/bctend.x > bctend.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME
#end of EXEC_BCTEND
#------------------------------------

export tmmark=tm00    ##### set back tmmark to tm00 #####

# do a three hour forecast from tmmark to tmmark+3

cp ${RUN_EDAS}/restrt00.tm00 restrt00

cp $UCL/bcexdata.parm.${tmmark} bcexdata.parm
cp $UCL/template.para.${tmmark}_f90 template
cp $UCL/namelist_filename.txt .
cp $UCL/soil_veg_namelist_ver_2.1 .

cp $UCL/fcstdata.parm.${tmmark}_freefcst fcstdata.parm
cp $UCL/adjppt.parm_yes adjppt.parm

##########################
#./EXEC_ETA2

rm -f fort.*

cat << EOF > ijtest
56 31
EOF

ln -s -f fcstdata.parm               fort.11
ln -s -f ${INPUTNARR}/nhb/nhb3245    fort.12
ln -s -f restrt00                    fort.13
ln -s -f $FIX32/radco2               fort.14
ln -s -f $FIX/staids_prof            fort.15
ln -s -f nbcout                      fort.16
ln -s -f bcexdata.parm               fort.17
ln -s -f $UCL/solar                  fort.19
ln -s -f template                    fort.21
ln -s -f $FIX32/z0eff                fort.22
ln -s -f $FIX32/hgtsmref.r3245.2d_tmasks        fort.30
### ln -s -f pptdata.${tmmark}           fort.41   no precip assim free forecast
ln -s -f profil                      fort.75
ln -s -f pro_c1                      fort.76
ln -s -f ppt.out.${tmmark}           fort.98

ln -s -f adjppt.parm                 fort.9
ln -s -f snow.parm                   fort.10
ln -s -f ${SNOW32}/snowd.nh.${YEAR}  fort.42
ln -s -f ${ICE32}/ice.${YEAR}.grb    fort.43
ln -s -f ${SST32}/sst.${YEAR}.grb    fort.44
ln -s -f ${REINIT}/albedo.grb        fort.45
ln -s -f ${REINIT}/veg.grb           fort.46


export pgm="`basename $EXEC/etafcst6x6.x .x`"
poe $EXEC/etafcst6x6.x < ijtest > etafcst.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME
##########################

cp $UCL/rstupt.parm_cycsnow snow.parm

#
# cleanup run_fcst directory
#
rm -f bcexdata.parm
rm -f curdate
rm -f date*
rm -f etabcs00
rm -f fcstdata.parm
rm -f fort.*
rm -f restrt*.*.tm*
rm -f restrt00
rm -f ijtest n09* n10*
rm -f namelist_filename.txt nbc3245 nbcout nfc3245
rm -f outjob*
rm -f rvetalbc
rm -f snow.parm soil_veg_namelist_ver_2.1 spectral_coefficients
rm -f template transmittance_coefficients umsu*

#
# remove RUN_EDAS=${TMPNARR}/run_${YEAR}/edas_restrt00.tm00.${DATE}
#
rm -rf ${TMPNARR}/run_${YEAR}/edas_restrt00.tm00.${DATE}

DM1=`ndate -12 $DATE`
YYYY=`expr substr $DM1 1 4`
rm -rf ${TMPNARR}/run_${YYYY}/edas_restrt00.tm00.${DM1}

DM1=`ndate -24 $DATE`
YYYY=`expr substr $DM1 1 4`
rm -rf ${TMPNARR}/run_${YYYY}/edas_restrt00.tm00.${DM1}

DM1=`ndate -36 $DATE`
YYYY=`expr substr $DM1 1 4`
rm -rf ${TMPNARR}/run_${YYYY}/edas_restrt00.tm00.${DM1}

DM1=`ndate -48 $DATE`
YYYY=`expr substr $DM1 1 4`
rm -rf ${TMPNARR}/run_${YYYY}/edas_restrt00.tm00.${DM1}


### Get ready for next cycle
DATE=`$UTIL/ndate +60 $DATE`

LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE_fcst`

################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################

echo "$0 ENDDATE "`date`

exit 0
