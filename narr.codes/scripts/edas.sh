#! /bin/sh

# @ step_name = edas
# @ output = job.$(jobid).$(stepid).$(step_name).out
# @ error = job.$(jobid).$(stepid).$(step_name).err
# @ job_type = parallel
# @ total_tasks = 119
#
#### @ node = 40
#### @ node_usage = not_shared
#
# @ blocking = unlimited
#
# @ wall_clock_limit = 00:20:00
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
mkdir -p $RUN_EDAS
cd $RUN_EDAS
rm -f $RUN_EDAS/*

echo $DATE > curdate
echo "$STREAM $DATE edas        `date`"> ${INIDIR}/curdate_edas
. ${NARR}/scr/narr_setdate.sh

echo DATEXX$DATE > nmcdate.tm00

echo " -----------------------------------------------"
echo "   North American Regional Reanalysis"
echo "   NCEP/EMC/MMB / ROOM 207 / 763-8000"
echo " "
echo "   ETA 3DVAR ANALYSIS TM12 TIME IS ${DATM12}"
echo "   `date`"
echo " _______________________________________________"
echo "                                                "
echo "environment info for this execution follows:"
echo "stream ..................................... $STREAM"
echo "temporary processing file directory is ..... $RUN_EDAS"
echo "executable UOI directory is ................ $EXEC"
echo "Unix Control Language OI directory is ...... $UCL"
echo "............................................"

echo READY

YEARM12=`expr substr $DATM12 1 4`
cp ${HOLDIN}/r2_sigma/${YEARM12}/sig.anl.$DATM12.ieee     sges
# cp ${HOLDIN}/r2_sigma/${YEAR}/sig.anl.$DATM12.ieee     avnbc0
# cp ${HOLDIN}/r2_sigma/${YEAR}/sig.anl.$DATM06.ieee     avnbc1
# cp ${HOLDIN}/r2_sigma/${YEAR}/sig.anl.$DATE.ieee       avnbc2
cp ${HOLDIN}/r2_sfcanl/${YEARM12}/sfc.anl.${DATM12}.ieee sfcanl

YEARM12=`expr substr $DATM12 1 4`

cp ${HOLDBUFR}/${YEARM12}/prepbufr2.$DATM12 prepcq.$DATM12
cp ${HOLDBUFR}/${YEARM12}/prepbufr2.$DATM09 prepcq.$DATM09
cp ${HOLDBUFR}/${YEARM12}/prepbufr2.$DATM06 prepcq.$DATM06
cp ${HOLDBUFR}/${YEARM12}/prepbufr2.$DATM03 prepcq.$DATM03
cp ${HOLDBUFR}/${YEAR}/prepbufr2.$DATE   prepcq.$DATE

# get precip

if [ $DOPCP = YES ]; then
   cp ${PCPDATA8}/${DATE}/egrd80.tm12.3hr .
   cp ${PCPDATA8}/${DATE}/egrd80.tm09.3hr .
   cp ${PCPDATA8}/${DATE}/egrd80.tm06.3hr .
   cp ${PCPDATA8}/${DATE}/egrd80.tm03.3hr .
fi

if [ $DORAD = YES ]; then

#   cp ${UCLrad}/eta_satinfo.txt satinfo
   cp ${UCLrad}/most_of_satinfo satinfo  # NEW

   #obname="n11h1b n11m1b n12h1b n12m1b n14h1b n14m1b"
   #obname="n11h1b n11m1b n12m1b n14h1b n14m1b"
    obname="h1bn05 h1bn06 h1bn07 h1bn08 h1bn09 h1bn10 h1bn11 h1bn12 h1bn14 h1bn15 h1bn16 \
            m1bn05 m1bn06 m1bn07 m1bn08 m1bn09 m1bn10 m1bn11 m1bn12 m1bn14 m1bn15 m1bn16"
   #obname="n09h1b n09m1b n10h1b n10m1b n11h1b n11m1b"

   MM=`expr substr $DATM12 5 2`
   for name in $obname
   do
      [ -f ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM12 ] && \
        cp ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM12 ${name}.tm12
      [ -f ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM09 ] && \
        cp ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM09 ${name}.tm09
      [ -f ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM06 ] && \
        cp ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM06 ${name}.tm06
      [ -f ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM03 ] && \
        cp ${DIR1B}/${YEARM12}/${MM}/${name}.$DATM03 ${name}.tm03
      [ -f ${DIR1B}/${YEAR}/${MM}/${name}.$DATE ] && \
        cp ${DIR1B}/${YEAR}/${MM}/${name}.$DATE ${name}.tm00   # for free forecast
   done

fi

#
#### Copy previous cycle tm03 3-h forecast for this cycle's first guess
#

if [ -r ${HOLD32}/nmcdate.tm12 ] && \
   [ -r ${HOLD32}/restrt03.edas ] && \
   [ -r ${HOLD32}/satbias_in ] ; then

##   [ -r ${HOLD32}/nhb3245.prev ] && \

   cp ${HOLD32}/nmcdate.tm12 nmcdate.tm12
   cp ${HOLD32}/restrt03.edas restrt03
   cp ${HOLD32}/restrt03.edas restrt03.edas
##   cp ${HOLD32}/nhb3245.prev nhb3245
##   cp ${HOLD32}/nhb3245.prev nhb3245.prev
   cp ${HOLD32}/satbias_in satbias_in

   if [ "`cut -c 7-16 nmcdate.tm12`" != "${DATM12}" ]; then
      echo "ERROR: nmcdate.tm12 from hold32 not equal date-12"
      echo "ERROR: nmcdate.tm12 from hold32 not equal date-12"
      echo "ERROR: nmcdate.tm12 from hold32 not equal date-12"
      echo "ERROR: nmcdate.tm12 from hold32 not equal date-12"
      export err=1
      export pgm="nmcdate.tm12 from hold32 not equal date-12"
      ${NARR}/scr/err_chk $LOADL_JOB_NAME
   else
      echo "nmcdate.tm12 from hold32 is equal date-12; OK"
   fi

   export GUESS=ETA
   echo $GUESS > guess

else

   echo "*****************************************************************"
   echo "*****************************************************************"
   echo "***                                                           ***"
   echo "***                C O L D    S T A R T                       ***"
   echo "***                                                           ***"
   echo "*****************************************************************"
   echo "*****************************************************************"
   export GUESS=GBL
   echo $GUESS > guess

   #------------------------------------
   #./EXEC_GESP
   rm -f fort*
   ln -s -f sges                  fort.11
   ln -s -f $FIX32/newtopo.r3245  fort.13
   ln -s -f gesgrd                fort.51
   ln -s -f errgrd                fort.52
   ln -s -f grdspecs              fort.53
   ln -s -f rvspecs2              fort.54
   ln -s -f rvguess               fort.55
   export pgm=`basename $EXEC/gesprep.x .x`
   $EXEC/gesprep.x < $UCL/gesprep.parm > gesprep.out
   export err=$?
   ${NARR}/scr/err_chk $LOADL_JOB_NAME
   rm rvguess
   #end of EXEC_GESP
   #------------------------------------


   #------------------------------------
   #./EXEC_GRID

   rm -f fort.*
   ln -s -f gesgrd                                 fort.11
   ln -s -f grdspecs                               fort.13
   ln -s -f $FIX32/hgtsmref.r3245.2d_tmasks        fort.14
   ln -s -f $UCL/econdata.parm                     fort.15
   ln -s -f $FIX32/deta_ldt1.45.25mb               fort.16
   ln -s -f $UCL/updtsfc.parm                      fort.17
   ln -s -f $FIX/maxsnoalb.bin                     fort.20
   ln -s -f $FIX/alb1_ieee                         fort.21
   ln -s -f $FIX/alb2_ieee                         fort.22
   ln -s -f $FIX/alb3_ieee                         fort.23
   ln -s -f $FIX/alb4_ieee                         fort.24
   ln -s -f $FIX/ivgtyp_1d_ieee                    fort.30
   ln -s -f $FIX/isltyp_1d_ieee                    fort.31
   ln -s -f $FIX/islope_1d_ieee                    fort.32
   ln -s -f sfcanl                                 fort.33
   ln -s -f $SST32/sst.$YEAR.grb                   fort.39
   ln -s -f $FIX/veg.eta.grb                       fort.40
   ln -s -f $SNOW32/snowd.nh.${YEAR}               fort.41
   ln -s -f $FIX/rfusaflw                          fort.42
   ln -s -f $FIX/imsmask                           fort.43
   ln -s -f $ICE32/ice.${YEAR}.grb                 fort.44
   #ln -s -f nhb3245.prev                           fort.48
   #ln -s -f restrt03.edas                          fort.49
   ln -s -f ${INPUTNARR}/para32restrt/restrt03.gdas       fort.46
   ln -s -f ${INPUTNARR}/oct87/restrt03.edas       fort.47
   ln -s -f ${INPUTNARR}/nhb/nhb3245               fort.48
   ln -s -f ${INPUTNARR}/oct88/restrt03.edas       fort.49
   ln -s -f eta3245                                fort.51
   ln -s -f date                                   fort.52
   ln -s -f nfc3245                                fort.53
   ln -s -f nhb3245                                fort.54
   ln -s -f nbc3245                                fort.55
   ln -s -f restrt03.gdas                          fort.56
   export pgm="`basename $EXEC/grdeta.x .x`"
   $EXEC/grdeta.x <$UCL/modtop.parm > grdeta.out
   export err=$?
   ${NARR}/scr/err_chk $LOADL_JOB_NAME

   mv restrt03.gdas restrt03

   #
   # save nhb file, all subsequent programs will point to ${INPUTNARR}/nhb/nhb3245
   #
   mv nhb3245 ${INPUTNARR}/nhb/nhb3245

   #end of EXEC_GRID
   #------------------------------------

fi

if [ $CYC = 12 ]; then
   cp $UCL/rstupt.parm_newsnow snow.parm
else
   cp $UCL/rstupt.parm_cycsnow snow.parm
fi

date

#-------------------------------------------------------------------------------
for tmmark in tm12 tm09 tm06 tm03; do
#-------------------------------------------------------------------------------

export tmmark
export itag=03
export restrtahr=03

case $tmmark in
   tm12) DATMMM=$DATM12 ;;
   tm09) DATMMM=$DATM09 ;;
   tm06) DATMMM=$DATM06 ;;
   tm03) DATMMM=$DATM03 ;;
   tm00) DATMMM=$DATE ;;
esac

if [ $DOOBS = YES ]; then
   cp prepcq.$DATMMM prepcq
fi

if [ $DORAD = YES ]; then
   [ -f h1bn05.${tmmark} ] && cp h1bn05.${tmmark} hirs1b5
   [ -f m1bn05.${tmmark} ] && cp m1bn05.${tmmark} umsu1b5
   [ -f h1bn06.${tmmark} ] && cp h1bn06.${tmmark} hirs1b6
   [ -f m1bn06.${tmmark} ] && cp m1bn06.${tmmark} umsu1b6
   [ -f h1bn07.${tmmark} ] && cp h1bn07.${tmmark} hirs1b7
   [ -f m1bn07.${tmmark} ] && cp m1bn07.${tmmark} umsu1b7
   [ -f h1bn08.${tmmark} ] && cp h1bn08.${tmmark} hirs1b8
   [ -f m1bn08.${tmmark} ] && cp m1bn08.${tmmark} umsu1b8
   [ -f h1bn09.${tmmark} ] && cp h1bn09.${tmmark} hirs1b9
   [ -f m1bn09.${tmmark} ] && cp m1bn09.${tmmark} umsu1b9
   [ -f h1bn10.${tmmark} ] && cp h1bn10.${tmmark} hirs1b10
   [ -f m1bn10.${tmmark} ] && cp m1bn10.${tmmark} umsu1b10
   [ -f h1bn11.${tmmark} ] && cp h1bn11.${tmmark} hirs1b11
   [ -f m1bn11.${tmmark} ] && cp m1bn11.${tmmark} umsu1b11
   [ -f h1bn12.${tmmark} ] && cp h1bn12.${tmmark} hirs1b12
   [ -f m1bn12.${tmmark} ] && cp m1bn12.${tmmark} umsu1b12
   [ -f h1bn14.${tmmark} ] && cp h1bn14.${tmmark} hirs1b14
   [ -f m1bn14.${tmmark} ] && cp m1bn14.${tmmark} umsu1b14
   [ -f h1bn15.${tmmark} ] && cp h1bn15.${tmmark} hirs1b15
   [ -f m1bn15.${tmmark} ] && cp m1bn15.${tmmark} umsu1b15
   [ -f h1bn16.${tmmark} ] && cp h1bn16.${tmmark} hirs1b16
   [ -f m1bn16.${tmmark} ] && cp m1bn16.${tmmark} umsu1b16
fi

if [ $DOPCP = YES ]; then
   cp egrd80.${tmmark}.3hr pptdata.${tmmark}
fi

cp $UCL/fcstdata.parm.${tmmark}_3dvar fcstdata.parm

#------------------------------------
#./EXEC_ANAL
rm -f fort.*

mv restrt03 restrt03.ges

# spec_coefs=$UCLrad/POES_9_12_spectral_coefficients.big_endian
# trans_coefs=$UCLrad/POES_9_12_transmittance_coefficients.big_endian

spec_coefs=$UCLrad/spectral_coefficients.most_of_satinfo
trans_coefs=$UCLrad/transmittance_coefficients.most_of_satinfo

ln -s -f $FIX32/eta3245_globstats       fort.9
ln -s -f fcstdata.parm                  fort.11
ln -s -f ${INPUTNARR}/nhb/nhb3245       fort.12
ln -s -f restrt03.ges                   fort.13
ln -s -f $UCLrad/r3dvar.parm32          fort.15
ln -s -f sges.3dvar                     fort.16
ln -s -f $UCL/errtable.r3dv             fort.19
ln -s -f $FIX32/z0eff                   fort.22
ln -s -f prepcq                         fort.30
ln -s -f $UCL/etaerr1780_ieee           fort.47
ln -s -f restrt03                       fort.51
ln -s -f nfc3245                        fort.52
ln -s -f nbc3245                        fort.53

ln -s -f snow.parm                       fort.10
ln -s -f $FIX32/hgtsmref.r3245.2d_tmasks fort.41
ln -s -f ${SNOW32}/snowd.nh.${YEARM12}   fort.42
ln -s -f ${ICE32}/ice.${YEARM12}.grb     fort.43
ln -s -f ${SST32}/sst.${YEARM12}.grb     fort.44
ln -s -f ${REINIT}/albedo.grb            fort.45
ln -s -f ${REINIT}/veg.grb               fort.46

ln -s -f $spec_coefs                    spectral_coefficients
ln -s -f $trans_coefs                   transmittance_coefficients

set +e
export pgm="`basename $EXEC/r3dvar.x .x`"
poe $EXEC/r3dvar.x > r3dvar.out.${tmmark}
export err=$?

if [ $err -ne 0 ]; then

   echo `date`" !!!!!!!!!!! r3dvar crashed !!!!!!!!!!! ${DATE}"
   echo `date`" !!!!!!!!!!! r3dvar crashed !!!!!!!!!!! ${DATE}"
   echo `date`" !!!!!!!!!!! r3dvar crashed !!!!!!!!!!! ${DATE}" >> ${TMPNARR}/CRASHED
   echo `date`" !!!!!!!!!!! r3dvar crashed !!!!!!!!!!! ${DATE}" >> ${TMPNARR}/CRASHED

   mv r3dvar.out.${tmmark} r3dvar.out.${tmmark}.crash
   poe $EXEC/r3dvar_notraps.x > r3dvar.out.${tmmark}
   export err=$?

fi
set -e

${NARR}/scr/err_chk $LOADL_JOB_NAME

cat fit* > fits.all
cat diag_rad1* > radstats
rm -f diag_rad1*
cp fort.207 radiance.out

rm restrt03.ges

#end of EXEC_ANAL
#------------------------------------

cp satbias_out satbias_in
mv fits.all fits.all.${tmmark}
mv radstats radstats.$DATMMM
mv radiance.out radiance.out.${tmmark}

cat events9*>allevents
sed '1,109d' allevents > allevents.tmp

rm -f fort.*
ln -sf allevents.tmp           fort.20
ln -sf prepcq                  fort.21
ln -sf prepout.$DATMMM         fort.50
$EXEC/rrvents.x > rrvents.out.$DATMMM

set +e
grep ">>>>>>>>>>" rrvents.out.$DATMMM
found=$?
if [ $found -eq 0 ]; then
echo "checksum error or uv event error for rrvents.out.$DATMMM" >> ${TMPNARR}/ERROR.checksum
else
echo "rrvents OK"
fi

### Running of Jack's cmpevn code to produce output for QC monitoring

mkdir -p ${TMPNARR}/cmpevn
echo prepout.$DATMMM | $EXEC/cmpevn.x > ${TMPNARR}/cmpevn/cmpevn.$DATMMM

set -e

compress allevents
mv allevents.Z allevents.${tmmark}.Z
rm events*
rm allevents.tmp
rm -f radar_cut1*
rm -f radar_remaining_*

##### compute boundary conditions
#
#    if [ ${tmmark} = tm12 ]; then
#    #------------------------------------
#    #./EXEC_BND_EDAS
#    rm -f fort.*
#
#    ln -s -f avnbc0                    fort.11
#    ln -s -f avnbc1                    fort.12
#    ln -s -f avnbc2                    fort.13
#    ln -s -f nmcdate                   fort.40
#    ln -s -f $UCL/modtop.parm          fort.41
#    ln -s -f $FIX32/deta_ldt1.45.25mb  fort.42
#    ln -s -f rvetalbc                  fort.51
#    ln -s -f etabcs00                  fort.52
#
#    export pgm="`basename $EXEC/mkbnd.x .x`"
#    $EXEC/mkbnd.x < $UCL/mkbnd.parm_3 > mkbnd.out.${tmmark}
#    export err=$?
#
#    rm avnbc*
#    #end of EXEC_BND_EDAS
#    #------------------------------------
#    fi
#
#------------------------------------
#./EXEC_BCTEND
rm -f fort.*

#ln -s -f etabcs00  fort.17
ln -s -f ${INPUTNARR}/etabcs/${YEAR}/etabcs00_${DATE} fort.17
ln -s -f nbc3245   fort.18
ln -s -f nbcout    fort.51
export pgm="`basename $EXEC/bctend.x .x`"
$EXEC/bctend.x > bctend.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME
#end of EXEC_BCTEND
#------------------------------------

cp nbcout nbcout.${tmmark}

# do a three hour forecast from tmmark to tmmark+3

cp $UCL/bcexdata.parm.${tmmark} bcexdata.parm
cp $UCL/template.para.${tmmark}_f90 template
cp $UCL/namelist_filename.txt .
cp $UCL/soil_veg_namelist_ver_2.1 .

GUESS=`cat guess`
if [ $GUESS = ETA ]; then
   cp $UCL/fcstdata.parm.${tmmark}_etag fcstdata.parm
else
   cp $UCL/fcstdata.parm.${tmmark} fcstdata.parm
fi

###if [ ${DATE} -ge 1988080112 -a ${DATE} -le 1988080600 ]; then
###   cp $UCL/adjppt.parm_no adjppt.parm
###   echo " NO PRECIPITATION ASSIMILATION !!!"
###   echo " NO PRECIPITATION ASSIMILATION !!!"
###   echo " NO PRECIPITATION ASSIMILATION !!!"
###else
   cp $UCL/adjppt.parm_yes adjppt.parm
###fi

##########################
#./EXEC_ETA2

rm -f fort.*

cat << EOF > ijtest
56 31
EOF

ln -s -f fcstdata.parm               fort.11
ln -s -f ${INPUTNARR}/nhb/nhb3245    fort.12
ln -s -f restrt03                    fort.13
ln -s -f $FIX32/radco2               fort.14
ln -s -f $FIX/staids_prof            fort.15
ln -s -f nbcout                      fort.16
ln -s -f bcexdata.parm               fort.17
ln -s -f $UCL/solar                  fort.19
ln -s -f template                    fort.21
ln -s -f $FIX32/z0eff                fort.22
ln -s -f $FIX32/hgtsmref.r3245.2d_tmasks        fort.30
ln -s -f pptdata.${tmmark}           fort.41
ln -s -f profil                      fort.75
ln -s -f pro_c1                      fort.76
ln -s -f ppt.out.${tmmark}           fort.98

ln -s -f adjppt.parm                 fort.9
ln -s -f snow.parm                   fort.10
ln -s -f ${SNOW32}/snowd.nh.${YEARM12}  fort.42
ln -s -f ${ICE32}/ice.${YEARM12}.grb    fort.43
ln -s -f ${SST32}/sst.${YEARM12}.grb    fort.44
ln -s -f ${REINIT}/albedo.grb        fort.45
ln -s -f ${REINIT}/veg.grb           fort.46


export pgm="`basename $EXEC/etafcst.x .x`"
poe $EXEC/etafcst.x < ijtest > etafcst.out.${tmmark}
export err=$?
${NARR}/scr/err_chk $LOADL_JOB_NAME
##########################

cp $UCL/rstupt.parm_cycsnow snow.parm

cp restrt03.${tmmark} restrt03

# save guess for next analysis

if [ ${tmmark} = tm03 ]; then
   mkdir -p ${HOLD32}
   cp restrt03.tm03 ${HOLD32}/restrt03.edas
#   cp nhb3245       ${HOLD32}/nhb3245.prev
   cp nmcdate.tm00  ${HOLD32}/nmcdate.tm12
   cp satbias_in    ${HOLD32}/satbias_in
fi

#
# Save tm12 analysis for free forecast run
#
if [ ${tmmark} = tm12 ]; then
   mkdir -p ${TMPNARR}/run_${YEARM12}/edas_restrt00.tm00.${DATM12}
   cp restrt00.tm12 ${TMPNARR}/run_${YEARM12}/edas_restrt00.tm00.${DATM12}/restrt00.tm00
   cp nbc3245 ${TMPNARR}/run_${YEARM12}/edas_restrt00.tm00.${DATM12}/nbc3245
   echo "DONE" > ${TMPNARR}/run_${YEARM12}/edas_restrt00.tm00.${DATM12}/fcstdone00.tm00
fi

#-------------------------------------------------------------------------------
done  ######### for tmmark in tm12 tm09 tm06 tm03; do
#-------------------------------------------------------------------------------

mkdir -p ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}
rm -f ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}/*

cp restrt03.tm03   ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}
cp prepcq.$DATE    ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}
cp sges.3dvar      ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}
cp fcstdone03.tm03 ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}
cp satbias_out     ${TMPNARR}/run_${YEAR}/edas03tm03_${DATE}

#
# cleanup run_edas directory
#
rm -f 3dvrdone*
rm -f SR.tm*
rm -f amsu*
rm -f bcexdata.parm
rm -f curdate
rm -f date*
rm -f egrd80*
rm -f eta3245
rm -f etabcs00
rm -f fcstdata.parm
rm -f fith fitpw fitq fitradarw fitt fitw
rm -f fort.*
rm -f h1bn* m1bn* hirs*
rm -f pptdata.tm*
rm -f prepcq
rm -f restrt*.*.tm*
rm -f restrt03 restrt03.edas
rm -f gesgrd
rm -f grdspecs guess
rm -f ijtest n09* n10*
rm -f namelist_filename.txt nbc3245 nbcout nfc3245
rm -f outjob00 outjob03
rm -f rvetalbc
rm -f satbias_in satbias_pred satinfo
rm -f sfcanl sges* snow.parm soil_veg_namelist_ver_2.1 spectral_coefficients
rm -f template transmittance_coefficients umsu*

### Get ready for next cycle
DATE=`$UTIL/ndate +12 $DATE`

LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE`

################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################

echo "$0 ENDDATE "`date`

exit 0
