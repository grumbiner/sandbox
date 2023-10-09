#!/bin/ksh
#####################################################################
echo "------------------------------------------------"
echo "Concentration Fields for the Arctic Ocean and Southern"
echo "Continent and adjoining water bodies."
#####################################################################
# 11 January 2023 -- major overhaul to do satellite-by-satellite processing
########################################################################

set -xe

### Definition of the Fix fields:
export FNLAND=${FNLAND:-${FIXseaice_amsr2}/seaice_nland.map}
export FNLAND127=${FNLAND127:-${FIXseaice_amsr2}/seaice_nland127.map}
export FSLAND=${FSLAND:-${FIXseaice_amsr2}/seaice_sland.map}
export FSLAND127=${FSLAND127:-${FIXseaice_amsr2}/seaice_sland127.map}
export FGLAND5MIN=${FGLAND5MIN:-${FIXseaice_amsr2}/seaice_gland5min}
export FPOSTERIORI5=${FPOSTERIORI5:-${FIXseaice_amsr2}/seaice_posteriori_5min}
export FPOSTERIORI30=${FPOSTERIORI30:-${FIXseaice_amsr2}/seaice_posteriori_30min}
export FDIST=${FDIST:-${FIXseaice_amsr2}/seaice_alldist.bin}
export FGSHHS=${FGSHHS:-${FIXseaice_amsr2}/seaice_lake_isleout}

cd $DATA

##############################
#
# START FLOW OF CONTROL
#
# 1) Get the Date from /com/date
#    -- Now done in J-job, not here.
# 2) Run the dumpscript to retrieve a day's data
# 3) Process the bufr output into something readable
# 4) Run the analysis on the files
# -- 2,3,4 are repeated for each satellite being used
# 5) Copy the base analyst's (L3) grids to the running location
#
##############################

########################################
msg="HAS BEGUN!"
postmsg "$msg"
########################################

mailbody=mailbody.txt
rm -f $mailbody

#----------------------------------------------------------
#Begin ordinary processing.
#---------------------------------------------------------

#BUFR------------------------------------------------------
#Run the dumpscript to retrieve a day's data
#----------------------------------------------------------
#Cactus:
#if [ -z $obsproc_dump_ver ] ; then
#  echo null obsproc_dump_ver
#  export obsproc_dump_ver=v4.0.0
#  export obsproc_shared_bufr_dumplist_ver=v1.4.0
#fi

export pgm=dumpjb
. prep_step
#$USHobsproc_dump/dumpjb ${PDY}00 12 amsr2
$DUMPJB ${PDY}00 12 amsr2
export err=$?
if [ $err -ne 0 ] ; then
  msg="Cannot continue jseaice_amsr2 without amsr2 data"
  postmsg "$msg"
  echo "********************************************************************" >> $mailbody
  echo "*** WARNING:  dumpjb returned status $err.  Cannot continue without amsr2. " >> $mailbody
  echo "********************************************************************" >> $mailbody
  SENDEMAIL="YES"
fi
touch amsr2.ibm # ensures that file exists in rare event that dump is empty

# send email if any dumpjb calls returned non-zero status
  if [ -s "$mailbody" ] && [ "$SENDEMAIL" = "YES" ]; then
     subject="$job degraded due to missing data"
     mail.py -s "$subject" < $mailbody
  fi

#--------------------------------------------------------------------
#Process the bufr output into something readable: bufr --> l1b
#--------------------------------------------------------------------

##################Decode AMSR2 data
export pgm=seaice_amsrbufr
. prep_step

export XLFRTEOPTS="unit_vars=yes"
ln -sf amsr2.ibm fort.11
if [ -f fort.52 ] ; then
  rm fort.52
fi

startmsg
time $EXECseaice_amsr2/seaice_amsrbufr >> $pgmout 2> errfile
mv fort.52 amsr2.bufr
export err=$?;err_chk
if [ $err -ne 0 ] ; then
  msg="Cannot continue without amsr2bufr"
  postmsg "$msg"
  echo "********************************************************************" >> $mailbody
  echo "*** WARNING:  amsr2bufr returned status $err.  Cannot continue without amsr2bufr " >> $mailbody
  echo "********************************************************************" >> $mailbody
fi

#---------------------------------------------------------------------
# Translate from l1b to l2.nc 
# AMSR2 remains straight shot l1b to l3
# Robert Grumbine 9 April 2021
#---------------------------------------------------------------------

cp $FIXseaice_amsr2/seaice_TBthark.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBthant.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBowark.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBowant.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBfyark.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBfyant.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBccark.tab.amsr2 .
cp $FIXseaice_amsr2/seaice_TBccant.tab.amsr2 .

#----------------------------------------------------------
# Run the L2 --> L3 analysis on the AMSR files
# Input files =  amsr2.nc, $FIXseaice_amsr2/seaice_nland127.map, $FIXseaice_amsr2/seaice_sland127.map
# Output files = n3amsr.$PDY, s3amsr.$PDY, amsr2north.$PDY, amsr2south.$PDY
#New 1 May 2009 -- amsre
#New 16 Aug 2017 -- amsr2
#----------------------------------------------------------
#
#
#
export pgm=seaice_iceamsr2
. prep_step
startmsg
time $EXECseaice_amsr2/seaice_iceamsr2 amsr2.bufr $FNLAND127 $FSLAND127 \
           namsr2.$PDY samsr2.$PDY amsr2north6.$PDY amsr2south6.$PDY \
           $FGSHHS $FDIST >> $pgmout 2>errfile
export err=$?;err_chk


#-----------------------------------------------------------
#Copy the base analyst's grids to the running location
#-----------------------------------------------------------

if [ $SENDCOM = "YES" ]
then
  # Raw files -- L3 -- on per-instrument basis
#L3 with tb etc.
  cp namsr2.${PDY}_hr ${COMOUT}/seaice.t${cyc}z.namsr2.${PDY}_hr
  cp namsr2.${PDY}_lr ${COMOUT}/seaice.t${cyc}z.namsr2.${PDY}_lr
  cp samsr2.${PDY}_hr ${COMOUT}/seaice.t${cyc}z.samsr2.${PDY}_hr
  cp samsr2.${PDY}_lr ${COMOUT}/seaice.t${cyc}z.samsr2.${PDY}_lr
#L3 ice conc only
  cp amsr2north6.${PDY}   ${COMOUT}/seaice.t${cyc}z.amsr2north6.${PDY}
  cp amsr2south6.${PDY}   ${COMOUT}/seaice.t${cyc}z.amsr2south6.${PDY}
#L2 ice files
  cp l2out.* ${COMOUT}

fi

echo completed the L2 and L3 analyses


############## END OF SCRIPT #######################
