#! /bin/sh

# @ step_name = edas_verif
# @ output = job.$(jobid).$(stepid).$(step_name).out
# @ error = job.$(jobid).$(stepid).$(step_name).err
# @ job_type = parallel
# @ total_tasks = 4
#
#### @ node = 1
#### @ node_usage = not_shared
#
# @ blocking = unlimited
#
# @ wall_clock_limit = 00:30:00
# @ class = dev
#### @ group = devonprod
# @ network.MPI=csss,shared,us
# @ queue

#set -aeux
set -aux

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
export RUN_EDAS_VERIF=${TMPNARR}/run_${YEAR}/edas_verif_${DATE}
mkdir -p $RUN_EDAS_VERIF
cd $RUN_EDAS_VERIF

echo $DATE > curdate
echo "$STREAM $DATE edas_verif  `date`"> ${INIDIR}/curdate_edas_verif
. ${NARR}/scr/narr_setdate.sh

#---
ic=1
while [ $ic -lt 600 ] ; do
   if [ -s $RUN_EDAS_POST/eta_edas_post.$DATE ]; then
     echo "file eta_edas_post.$DATE exists"
     break
   else
     let "ic=ic+1"
     echo "sleep 10; wait on eta_edas_post.$DATE"
     sleep 10
   fi
   if [ $ic -ge 500 ]; then
     echo "NO eta_edas_post.$DATE FILE after 500 iterations"
     echo "NO eta_edas_post.$DATE FILE after 500 iterations"
     echo "NO eta_edas_post.$DATE FILE after 500 iterations"
     exit 1
   fi
done
#---

export HOLDOUT=${TMPNARR}/output_${YEAR}/edas_${DATE}
YEARM12=`expr substr $DATM12 1 4`
export HOLDOM12=${TMPNARR}/output_${YEARM12}/edas_${DATM12}

export HOLDPLOT=${TMPNARR}/output_${YEAR}/edas_plot_${DATE}
mkdir -p $HOLDPLOT

##################
## MONITORING   ##
##################

mkdir -p ${TMPNARR}/stats_test00
mkdir -p ${TMPNARR}/stats_test03

ls -l AWIP3D00.*
$UTIL/grbindex ${HOLDOUT}/AWIP3200.${DATM03} ${HOLDOUT}/AWIP3200i.${DATM03} &
$UTIL/grbindex ${HOLDOUT}/AWIP3200.${DATM06} ${HOLDOUT}/AWIP3200i.${DATM06} &
$UTIL/grbindex ${HOLDOUT}/AWIP3200.${DATM09} ${HOLDOUT}/AWIP3200i.${DATM09} &
$UTIL/grbindex ${HOLDOUT}/AWIP3200.${DATM12} ${HOLDOUT}/AWIP3200i.${DATM12} &
wait
$UTIL/grbindex ${HOLDOUT}/AWIP3203.${DATM03} ${HOLDOUT}/AWIP3203i.${DATM03} &
$UTIL/grbindex ${HOLDOUT}/AWIP3203.${DATM06} ${HOLDOUT}/AWIP3203i.${DATM06} &
$UTIL/grbindex ${HOLDOUT}/AWIP3203.${DATM09} ${HOLDOUT}/AWIP3203i.${DATM09} &
$UTIL/grbindex ${HOLDOUT}/AWIP3203.${DATM12} ${HOLDOUT}/AWIP3203i.${DATM12} &
wait

#######################
YYYY=`expr substr ${DATM12} 1 4`
MM=`expr substr ${DATM12} 5 2`
DD=`expr substr ${DATM12} 7 2`
HH=`expr substr ${DATM12} 9 2`
if [ ${DD} = "01" -a ${HH} = "00" ]; then
   mkdir -p ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}
   rm -f ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/*
fi

GRIBFILE=${HOLDOUT}/EGDAWP03.${DATM12}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/EGDAWP.APCP.${DATM12}
GRIBFILE=${HOLDOUT}/EGDAWP03.${DATM09}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/EGDAWP.APCP.${DATM09}
GRIBFILE=${HOLDOUT}/EGDAWP03.${DATM06}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/EGDAWP.APCP.${DATM06}
GRIBFILE=${HOLDOUT}/EGDAWP03.${DATM03}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/EGDAWP.APCP.${DATM03}

GRIBFILE=${HOLDOUT}/AWIP3203.${DATM12}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/AWIP32.APCP.${DATM12}
GRIBFILE=${HOLDOUT}/AWIP3203.${DATM09}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/AWIP32.APCP.${DATM09}
GRIBFILE=${HOLDOUT}/AWIP3203.${DATM06}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/AWIP32.APCP.${DATM06}
GRIBFILE=${HOLDOUT}/AWIP3203.${DATM03}
/u/wx51we/bin/wgrib ${GRIBFILE} | grep "kpds5=61:kpds6=1:kpds7=0" | /u/wx51we/bin/wgrib -i ${GRIBFILE} -grib -o ${TMPNARR}/output_${YYYY}/precip/${YYYY}${MM}/AWIP32.APCP.${DATM03}

#######################
# . ${NARR}/sorc/monitoring/plot_rr_r2.sh ${HOLDOUT}/AWIP3200.${DATM12}
# . ${NARR}/sorc/monitoring/plot_rr_r2.sh ${HOLDOUT}/AWIP3200.${DATM06}
#######################

typeset -L8 pll3
pll3=EDS

DATPREP=$DATM12
DATPBEG=$DATM12
ENDPREP=$DATM03
export DATPREP ENDPREP DATPBEG

while [ $DATPREP -le $ENDPREP ]; do

DATPREP03=`$UTIL/ndate +03 $DATPREP`
DATPREPM03=`$UTIL/ndate -03 $DATPREP`

## 4panel charts

. ${NARR}/sorc/monitoring/4panel.sh ${HOLDOUT}/AWIP3200.${DATPREP} ${HOLDOUT}/AWIP3203.${DATPREP} ${STREAM}

for pngfile in *.png
do
  mv ${pngfile} $HOLDPLOT/${DATPREP}.${pngfile}
done

##

cat<<eof>prepfits.in00
${pll3} ${HOLDOUT}/AWIP3200.$DATPREP
${pll3} ${HOLDOUT}/AWIP3200i.$DATPREP
eof

if [ $DATPREP -eq $DATPBEG ]; then
cat<<eof>prepfits.in03
${pll3} ${HOLDOM12}/AWIP3203.$DATPREPM03
${pll3} ${HOLDOM12}/AWIP3203i.$DATPREPM03
eof
else
cat<<eof>prepfits.in03
${pll3} ${HOLDOUT}/AWIP3203.$DATPREPM03
${pll3} ${HOLDOUT}/AWIP3203i.$DATPREPM03
eof
fi
#  -------------------------------------------------------------
#  run editbufr and prepfits on the combined set of observations
#  -------------------------------------------------------------

rm -f data00 data03 prepfits.ETA*


rm -f fort.*
#ln -s -f $RUN_EDAS/prepcq.$DATPREP         fort.20
ln -s -f $HOLDOUT/prepout.$DATPREP        fort.20
ln -s -f data00                            fort.50
$EXEC/editbufr.x < $UCLfits/keeplist.eta

rm -f fort.*
ln -s -f $UCLfits/levcat.eta               fort.11
ln -s -f data00                            fort.20
ln -s -f $UCLfits/prepfits.tab             fort.22
ln -s -f prepfits.ETA00.${DATPREP}         fort.50
$EXEC/prepfits.x < prepfits.in00 > prepfits.${DATPREP}_00.out
rm data00

rm -f fort.*
ln -s -f prepfits.ETA00.${DATPREP}         fort.10
ln -s -f $UCLfits/grid#104                 fort.20
ln -s -f $UCLfits/regions                  fort.21
ln -s -f eds_${DATPREP}.vdb                fort.50
$EXEC/gridtobs.x < $UCLfits/gridtobs.eds0012a > gridtobs.${DATPREP}_00.out


rm -f fort.*
#ln -s -f $RUN_EDAS/prepcq.$DATPREP03       fort.20
ln -s -f $HOLDOUT/prepout.$DATPREP         fort.20
ln -s -f data03                            fort.50
$EXEC/editbufr.x < $UCLfits/keeplist.eta

rm -f fort.*
ln -s -f $UCLfits/levcat.eta               fort.11
ln -s -f data03                            fort.20
ln -s -f $UCLfits/prepfits.tab             fort.22
ln -s -f prepfits.ETA03.${DATPREP}         fort.50
$EXEC/prepfits.x < prepfits.in03 > prepfits.${DATPREP}_03.out
rm data03

rm -f fort.*
ln -s -f prepfits.ETA03.${DATPREP}         fort.10
ln -s -f $UCLfits/grid#104                 fort.20
ln -s -f $UCLfits/regions                  fort.21
ln -s -f eds2_${DATPREP}.vdb               fort.50
$EXEC/gridtobs.x < $UCLfits/gridtobs.eds0012a > gridtobs.${DATPREP}_03.out


mv eds_${DATPREP}.vdb   ${TMPNARR}/stats_test00
mv eds2_${DATPREP}.vdb ${TMPNARR}/stats_test03

DATPREP=`$UTIL/ndate +03 $DATPREP`

rm -f fort.* prepfits.ETA*

done  ######### while [ $DATPREP -le $ENDPREP ]; do

if [ $CYC = 00 ]; then
cat ${TMPNARR}/stats_test00/eds_${CDAT1}*vdb > ${TMPNARR}/stats_test00/eds_${CDAT1}.vsdb
rm -f ${TMPNARR}/stats_test00/eds_${CDAT1}*vdb
cat ${TMPNARR}/stats_test03/eds2_${CDAT1}*vdb > ${TMPNARR}/stats_test03/eds_${CDAT1}.vsdb
rm -f ${TMPNARR}/stats_test03/eds2_${CDAT1}*vdb
fi

##################
## GRID-TO-GRID ##
##################

# YEAR4=`cut -c 1-4 curdate`
# MONTH=`cut -c 5-6 curdate`
# DAY=`cut -c 7-8 curdate`
# export YEAR4 MONTH DAY

export CYC=`cut -c 9-10 curdate`

export DATM12=`$UTIL/ndate -12 $DATE`
echo $DATM12 > curdate2
export YEARM1=`cut -c 1-4 curdate2`
export MONTHM1=`cut -c 5-6 curdate2`
export DAYM1=`cut -c 7-8 curdate2`
export HR=`cut -c 9-10 curdate2`

export DATM09=`$UTIL/ndate -09 $DATE`
echo $DATM09 > curdate3
export HR3=`cut -c 9-10 curdate3`

export DATM06=`$UTIL/ndate -06 $DATE`
export DATM03=`$UTIL/ndate -03 $DATE`

export AWPDIR=${HOLDOUT}
export AWPDIR98=${RUN_EDAS_VERIF}
mkdir -p $AWPDIR98
mkdir -p $TMPNARR/vsdb

$UTIL/copygb -xg98 $AWPDIR/AWIP3203.${DATM12} $AWPDIR98/AWIP3203.${DATM12}_g98 &
$UTIL/copygb -xg98 $AWPDIR/AWIP3203.${DATM09} $AWPDIR98/AWIP3203.${DATM09}_g98 &
$UTIL/copygb -xg98 $AWPDIR/AWIP3203.${DATM06} $AWPDIR98/AWIP3203.${DATM06}_g98 &
$UTIL/copygb -xg98 $AWPDIR/AWIP3203.${DATM03} $AWPDIR98/AWIP3203.${DATM03}_g98 &
wait

set -A mon jan feb mar apr may jun jul aug sep oct nov dec
let mn=MONTHM1-1
mname=${mon[mn]}
echo $mname

cat > awip3203cp_00-18.ctl << EOF
dset $AWPDIR98/AWIP3203.%y4%m2%d2%h2_g98
index ^AWIP3203cp_00-18.idx
undef 9.999E+20
title AWIP3203.1988073121_g98
*  produced by grib2ctl v0.9.12.5p32r
dtype grib 98
options yrev template
xdef 192 linear 0.000000 1.875
ydef 94 levels
 -88.542 -86.653 -84.753 -82.851 -80.947 -79.043 -77.139 -75.235 -73.331 -71.426
 -69.522 -67.617 -65.713 -63.808 -61.903 -59.999 -58.094 -56.189 -54.285 -52.380
 -50.475 -48.571 -46.666 -44.761 -42.856 -40.952 -39.047 -37.142 -35.238 -33.333
 -31.428 -29.523 -27.619 -25.714 -23.809 -21.904 -20.000 -18.095 -16.190 -14.286
 -12.381 -10.476  -8.571  -6.667  -4.762  -2.857  -0.952   0.952   2.857   4.762
   6.667   8.571  10.476  12.381  14.286  16.190  18.095  20.000  21.904  23.809
  25.714  27.619  29.523  31.428  33.333  35.238  37.142  39.047  40.952  42.856
  44.761  46.666  48.571  50.475  52.380  54.285  56.189  58.094  59.999  61.903
  63.808  65.713  67.617  69.522  71.426  73.331  75.235  77.139  79.043  80.947
  82.851  84.753  86.653  88.542
tdef 2 linear ${HR}Z${DAYM1}${mname}${YEARM1} 6hr
zdef 29 levels
1000 975 950 925 900 875 850 825 800 775 750 725 700 650 600 550 500 450 400 350 300 275 250 225 200 175 150 125 100
vars 181
no4LFTX180_0mb  0 132,116,46080 ** 180-0 mb above gnd Best (4-layer) lifted index [K]
ACPCPsfc  0 63,1, 0  ** surface Convective precipitation [kg/m^2]
ALBDOsfc  0 84,1, 0  ** surface Albedo [%]
APCPsfc  0 61,1, 0  ** surface Total precipitation [kg/m^2]
APCPNsfc  0 202,1, 0  ** surface Total precipitation (nearest grid point) [kg/m^2]
BGRUNsfc  0 234,1, 0  ** surface Subsurface runoff (baseflow) [kg/m^2]
BMIXLhbl  0 226,109,1  ** Blackadars mixing length scale [m]
CAPEsfc  0 157,1, 0  ** surface Convective available potential energy [J/kg]
CAPE180_0mb  0 157,116,46080 ** 180-0 mb above gnd Convective available potential energy [J/kg]
CCONDsfc  0 181,1, 0  ** surface Canopy conductance [m/s]
CDsfc  0 252,1, 0  ** surface Surface drag coefficient [non-dim]
CDCONclm  0 72,200,0 ** atmos column Convective cloud cover [%]
CDLYRclm  0 213,200,0 ** atmos column Non-convective cloud [%]
CFRZRsfc  0 141,1, 0  ** surface Categorical freezing rain [yes=1;no=0]
CICEPsfc  0 142,1, 0  ** surface Categorical ice pellets [yes=1;no=0]
CINsfc  0 156,1, 0  ** surface Convective inhibition [J/kg]
CIN180_0mb  0 156,116,46080 ** 180-0 mb above gnd Convective inhibition [J/kg]
CLWMRprs 29 153,100,0 ** Cloud water [kg/kg]
CNWATsfc  0 223,1, 0  ** surface Plant canopy surface water [kg/m^2]
CRAINsfc  0 140,1, 0  ** surface Categorical rain [yes=1;no=0]
CSNOWsfc  0 143,1, 0  ** surface Categorical snow [yes=1;no=0]
DLWRFsfc  0 205,1, 0  ** surface Downward longwave radiation flux [W/m^2]
DPT2m  0 17,105,2 ** 2 m Dew point temp. [K]
DSWRFsfc  0 204,1, 0  ** surface Downward shortwave radiation flux [W/m^2]
EVPsfc  0 57,1, 0  ** surface Evaporation [kg/m^2]
FRICVsfc  0 253,1, 0  ** surface Surface friction velocity [m/s]
GFLUXsfc  0 155,1, 0  ** surface Ground Heat Flux [W/m^2]
HCDChcl  0 75,234,0 ** high cloud level High level cloud cover [%]
HGTprs 29 7,100,0 ** Geopotential height [gpm]
HGThbl  0 7,109,1  ** Geopotential height [gpm]
HGTclb  0 7,2,0 ** cloud base Geopotential height [gpm]
HGTclt  0 7,3,0 ** cloud top Geopotential height [gpm]
HGT0deg  0 7,4,0 ** 0C isotherm level Geopotential height [gpm]
HGTmwl  0 7,6,0 ** max wind level Geopotential height [gpm]
HGTtrp  0 7,7,0 ** tropopause Geopotential height [gpm]
HLCYhlr  0 190,106,7680  ** Storm relative helicity [m^2/s^2]
HPBLsfc  0 221,1, 0  ** surface Planetary boundary layer height [m]
ICMRprs 29 178,100,0 ** Ice mixing ratio [kg/kg]
LCDClcl  0 73,214,0 ** low cloud level Low level cloud cover [%]
LFTX500_1000mb 0 131,101,12900 ** 500-1000 mb Surface lifted index [K]
LHTFLsfc  0 121,1, 0  ** surface Latent heat flux [W/m^2]
MCDCmcl  0 74,224,0 ** mid-cloud level Mid level cloud cover [%]
MCONVprs  0 135,100,850  ** Horizontal moisture divergence [kg/kg/s]
MCONVhbl  0 135,109,1  ** Horizontal moisture divergence [kg/kg/s]
MCONV30_0mb  0 135,116,7680 ** 30-0 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV60_30mb  0 135,116,15390 ** 60-30 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV90_60mb  0 135,116,23100 ** 90-60 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV120_90mb  0 135,116,30810 ** 120-90 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV150_120mb  0 135,116,38520 ** 150-120 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV180_150mb  0 135,116,46230 ** 180-150 mb above gnd Horizontal moisture divergence [kg/kg/s]
MSLETmsl  0 130,102,0  ** Mean sea level pressure (ETA model) [Pa]
MSTAVdlr  0 207,112,100  ** Moisture availability [%]
PEVAPsfc  0 228,1, 0  ** surface Potential evaporation [kg/m^2]
POTsfc  0 13,1, 0  ** surface Potential temp. [K]
POT10m  0 13,105,10 ** 10 m Potential temp. [K]
POT30m  0 13,105,30 ** 30 m Potential temp. [K]
POThbl  0 13,109,1  ** Potential temp. [K]
PRATEsfc  0 59,1, 0  ** surface Precipitation rate [kg/m^2/s]
PRESsfc  0 1,1, 0  ** surface Pressure [Pa]
PRES2m  0 1,105,2 ** 2 m Pressure [Pa]
PRES10m  0 1,105,10 ** 10 m Pressure [Pa]
PRES30m  0 1,105,30 ** 30 m Pressure [Pa]
PREShbl  0 1,109,1  ** Pressure [Pa]
PRESclb  0 1,2,0 ** cloud base Pressure [Pa]
PRESclt  0 1,3,0 ** cloud top Pressure [Pa]
PRESadcl  0 1,5,0 ** adiabatic lifting condensation level Pressure [Pa]
PRESmwl  0 1,6,0 ** max wind level Pressure [Pa]
PREStrp  0 1,7,0 ** tropopause Pressure [Pa]
PRESNsfc  0 134,1, 0  ** surface Pressure (nearest grid point) [Pa]
PRMSLmsl  0 2,102,0  ** Pressure reduced to MSL [Pa]
PWATclm  0 54,200,0 ** atmos column Precipitable water [kg/m^2]
RCQsfc  0 248,1, 0  ** surface Humidity parameter in canopy conductance [fraction]
RCSsfc  0 246,1, 0  ** surface Solar parameter in canopy conductance [fraction]
RCSOLsfc  0 249,1, 0  ** surface Soil moisture parameter in canopy conductance [fraction]
RCTsfc  0 247,1, 0  ** surface Temperature parameter in canopy conductance [fraction]
RH2m  0 52,105,2 ** 2 m Relative humidity [%]
RHhbl  0 52,109,1  ** Relative humidity [%]
RH0deg  0 52,4,0 ** 0C isotherm level Relative humidity [%]
SFEXCsfc  0 208,1, 0  ** surface Exchange coefficient [(kg/m^3)(m/s)]
SHTFLsfc  0 122,1, 0  ** surface Sensible heat flux [W/m^2]
SNODsfc  0 66,1, 0  ** surface Snow depth [m]
SNOHFsfc  0 229,1, 0  ** surface Snow phase-change heat flux [W/m^2]
SNOMsfc  0 99,1, 0  ** surface Snow melt [kg/m^2]
SNOWCsfc  0 238,1, 0  ** surface Snow cover [%]
SOILLSoilT  0 160,112,10 ** 0-10cm undergnd Liquid volumetric soil moisture (non-frozen) [fraction]
SOILLdlr 3 160,112,0 ** Liquid volumetric soil moisture (non-frozen) [fraction]
SOILMdlr  0 86,112,200  ** Soil moisture content [kg/m^2]
SOILWSoilT  0 144,112,10 ** 0-10cm undergnd Volumetric soil moisture (frozen + liquid) [fraction]
SOILWdlr 3 144,112,0 ** Volumetric soil moisture (frozen + liquid) [fraction]
SPFHprs 29 51,100,0 ** Specific humidity [kg/kg]
SPFH2m  0 51,105,2 ** 2 m Specific humidity [kg/kg]
SPFH10m  0 51,105,10 ** 10 m Specific humidity [kg/kg]
SPFH30m  0 51,105,30 ** 30 m Specific humidity [kg/kg]
SPFHhbl  0 51,109,1  ** Specific humidity [kg/kg]
SPFH30_0mb  0 51,116,7680 ** 30-0 mb above gnd Specific humidity [kg/kg]
SPFH60_30mb  0 51,116,15390 ** 60-30 mb above gnd Specific humidity [kg/kg]
SPFH90_60mb  0 51,116,23100 ** 90-60 mb above gnd Specific humidity [kg/kg]
SPFH120_90mb  0 51,116,30810 ** 120-90 mb above gnd Specific humidity [kg/kg]
SPFH150_120mb  0 51,116,38520 ** 150-120 mb above gnd Specific humidity [kg/kg]
SPFH180_150mb  0 51,116,46230 ** 180-150 mb above gnd Specific humidity [kg/kg]
SSRUNsfc  0 235,1, 0  ** surface Surface runoff (non-infiltrating) [kg/m^2]
TCDCclm  0 71,200,0 ** atmos column Total cloud cover [%]
TKEprs 15 158,100,0 ** Turbulent Kinetic Energy [J/kg]
TKEhbl  0 158,109,1  ** Turbulent Kinetic Energy [J/kg]
TMPsfc  0 11,1, 0  ** surface Temp. [K]
TMPprs 29 11,100,0 ** Temp. [K]
TMP2m  0 11,105,2 ** 2 m Temp. [K]
TMP10m  0 11,105,10 ** 10 m Temp. [K]
TMP30m  0 11,105,30 ** 30 m Temp. [K]
TMPhbl  0 11,109,1  ** Temp. [K]
TMP30_0mb  0 11,116,7680 ** 30-0 mb above gnd Temp. [K]
TMP60_30mb  0 11,116,15390 ** 60-30 mb above gnd Temp. [K]
TMP90_60mb  0 11,116,23100 ** 90-60 mb above gnd Temp. [K]
TMP120_90mb  0 11,116,30810 ** 120-90 mb above gnd Temp. [K]
TMP150_120mb  0 11,116,38520 ** 150-120 mb above gnd Temp. [K]
TMP180_150mb  0 11,116,46230 ** 180-150 mb above gnd Temp. [K]
TMPclt  0 11,3,0 ** cloud top Temp. [K]
TMPtrp  0 11,7,0 ** tropopause Temp. [K]
TSOILdpl  0 85,111,800  ** Soil temp. [K]
TSOILSoilT  0 85,112,10 ** 0-10cm undergnd Soil temp. [K]
TSOILdlr 3 85,112,0 ** Soil temp. [K]
UFLXsfc  0 124,1, 0  ** surface Zonal momentum flux [N/m^2]
UGRDprs 29 33,100,0 ** u wind [m/s]
UGRD10m  0 33,105,10 ** 10 m u wind [m/s]
UGRD30m  0 33,105,30 ** 30 m u wind [m/s]
UGRDhbl  0 33,109,1  ** u wind [m/s]
UGRD30_0mb  0 33,116,7680 ** 30-0 mb above gnd u wind [m/s]
UGRD60_30mb  0 33,116,15390 ** 60-30 mb above gnd u wind [m/s]
UGRD90_60mb  0 33,116,23100 ** 90-60 mb above gnd u wind [m/s]
UGRD120_90mb  0 33,116,30810 ** 120-90 mb above gnd u wind [m/s]
UGRD150_120mb  0 33,116,38520 ** 150-120 mb above gnd u wind [m/s]
UGRD180_150mb  0 33,116,46230 ** 180-150 mb above gnd u wind [m/s]
UGRDmwl  0 33,6,0 ** max wind level u wind [m/s]
UGRDtrp  0 33,7,0 ** tropopause u wind [m/s]
ULWRFsfc  0 212,1, 0  ** surface Upward long wave radiation flux [W/m^2]
ULWRFtoa  0 212,8,0 ** top of atmos Upward long wave radiation flux [W/m^2]
USTMhlr  0 196,106,15360  ** u-component of storm motion [m/s]
USWRFsfc  0 211,1, 0  ** surface Upward short wave radiation flux [W/m^2]
USWRFtoa  0 211,8,0 ** top of atmos Upward short wave radiation flux [W/m^2]
VEGsfc  0 87,1, 0  ** surface Vegetation [%]
VFLXsfc  0 125,1, 0  ** surface Meridional momentum flux [N/m^2]
VGRDprs 29 34,100,0 ** v wind [m/s]
VGRD10m  0 34,105,10 ** 10 m v wind [m/s]
VGRD30m  0 34,105,30 ** 30 m v wind [m/s]
VGRDhbl  0 34,109,1  ** v wind [m/s]
VGRD30_0mb  0 34,116,7680 ** 30-0 mb above gnd v wind [m/s]
VGRD60_30mb  0 34,116,15390 ** 60-30 mb above gnd v wind [m/s]
VGRD90_60mb  0 34,116,23100 ** 90-60 mb above gnd v wind [m/s]
VGRD120_90mb  0 34,116,30810 ** 120-90 mb above gnd v wind [m/s]
VGRD150_120mb  0 34,116,38520 ** 150-120 mb above gnd v wind [m/s]
VGRD180_150mb  0 34,116,46230 ** 180-150 mb above gnd v wind [m/s]
VGRDmwl  0 34,6,0 ** max wind level v wind [m/s]
VGRDtrp  0 34,7,0 ** tropopause v wind [m/s]
VISsfc  0 20,1, 0  ** surface Visibility [m]
VSTMhlr  0 197,106,15360  ** v-component of storm motion [m/s]
VVELprs 29 39,100,0 ** Pressure vertical velocity [Pa/s]
VVELhbl  0 39,109,1  ** Pressure vertical velocity [Pa/s]
VVEL30_0mb  0 39,116,7680 ** 30-0 mb above gnd Pressure vertical velocity [Pa/s]
VVEL60_30mb  0 39,116,15390 ** 60-30 mb above gnd Pressure vertical velocity [Pa/s]
VVEL90_60mb  0 39,116,23100 ** 90-60 mb above gnd Pressure vertical velocity [Pa/s]
VVEL120_90mb  0 39,116,30810 ** 120-90 mb above gnd Pressure vertical velocity [Pa/s]
VVEL150_120mb  0 39,116,38520 ** 150-120 mb above gnd Pressure vertical velocity [Pa/s]
VVEL180_150mb  0 39,116,46230 ** 180-150 mb above gnd Pressure vertical velocity [Pa/s]
VWSHtrp  0 136,7,0 ** tropopause Vertical speed shear [1/s]
WCCONVplr  0 241,101,70  ** Water condensate flux convergence (vertical int) [kg/m^2/s]
WCCONVclm  0 241,200,0 ** atmos column Water condensate flux convergence (vertical int) [kg/m^2/s]
WCINCplr  0 233,101,70  ** water condensate added by precip assimilaition [kg/m^2/s]
WCINCclm  0 233,200,0 ** atmos column water condensate added by precip assimilaition [kg/m^2/s]
WCUFLXplr  0 244,101,70  ** Water condensate zonal flux (vertical int) [kg/m/s]
WCUFLXclm  0 244,200,0 ** atmos column Water condensate zonal flux (vertical int) [kg/m/s]
WCVFLXplr  0 245,101,70  ** Water condensate meridional flux (vertical int) [kg/m/s]
WCVFLXclm  0 245,200,0 ** atmos column Water condensate meridional flux (vertical int) [kg/m/s]
WEASDsfc  0 65,1, 0  ** surface Accum. snow [kg/m^2]
WVCONVplr  0 237,101,70  ** Water vapor flux convergence (vertical int) [kg/m^2/s]
WVCONVclm  0 237,200,0 ** atmos column Water vapor flux convergence (vertical int) [kg/m^2/s]
WVINCplr  0 232,101,70  ** water vapor added by precip assimilation [kg/m^2/s]
WVINCclm  0 232,200,0 ** atmos column water vapor added by precip assimilation [kg/m^2/s]
WVUFLXplr  0 242,101,70  ** Water vapor zonal flux (vertical int)[kg/m/s]
WVUFLXclm  0 242,200,0 ** atmos column Water vapor zonal flux (vertical int)[kg/m/s]
WVVFLXplr  0 243,101,70  ** Water vapor meridional flux (vertical int) [kg/m/s]
WVVFLXclm  0 243,200,0 ** atmos column Water vapor meridional flux (vertical int) [kg/m/s]
ENDVARS
EOF

cat > awip3203cp_03-21.ctl << EOF
dset $AWPDIR98/AWIP3203.%y4%m2%d2%h2_g98
index ^AWIP3203cp_03-21.idx
undef 9.999E+20
title AWIP3203.1988073121_g98
*  produced by grib2ctl v0.9.12.5p32r
dtype grib 98
options yrev template
xdef 192 linear 0.000000 1.875
ydef 94 levels
 -88.542 -86.653 -84.753 -82.851 -80.947 -79.043 -77.139 -75.235 -73.331 -71.426
 -69.522 -67.617 -65.713 -63.808 -61.903 -59.999 -58.094 -56.189 -54.285 -52.380
 -50.475 -48.571 -46.666 -44.761 -42.856 -40.952 -39.047 -37.142 -35.238 -33.333
 -31.428 -29.523 -27.619 -25.714 -23.809 -21.904 -20.000 -18.095 -16.190 -14.286
 -12.381 -10.476  -8.571  -6.667  -4.762  -2.857  -0.952   0.952   2.857   4.762
   6.667   8.571  10.476  12.381  14.286  16.190  18.095  20.000  21.904  23.809
  25.714  27.619  29.523  31.428  33.333  35.238  37.142  39.047  40.952  42.856
  44.761  46.666  48.571  50.475  52.380  54.285  56.189  58.094  59.999  61.903
  63.808  65.713  67.617  69.522  71.426  73.331  75.235  77.139  79.043  80.947
  82.851  84.753  86.653  88.542
tdef 2 linear ${HR3}Z${DAYM1}${mname}${YEARM1} 6hr
zdef 29 levels
1000 975 950 925 900 875 850 825 800 775 750 725 700 650 600 550 500 450 400 350 300 275 250 225 200 175 150 125 100
vars 181
no4LFTX180_0mb  0 132,116,46080 ** 180-0 mb above gnd Best (4-layer) lifted index [K]
ACPCPsfc  0 63,1, 0  ** surface Convective precipitation [kg/m^2]
ALBDOsfc  0 84,1, 0  ** surface Albedo [%]
APCPsfc  0 61,1, 0  ** surface Total precipitation [kg/m^2]
APCPNsfc  0 202,1, 0  ** surface Total precipitation (nearest grid point) [kg/m^2]
BGRUNsfc  0 234,1, 0  ** surface Subsurface runoff (baseflow) [kg/m^2]
BMIXLhbl  0 226,109,1  ** Blackadars mixing length scale [m]
CAPEsfc  0 157,1, 0  ** surface Convective available potential energy [J/kg]
CAPE180_0mb  0 157,116,46080 ** 180-0 mb above gnd Convective available potential energy [J/kg]
CCONDsfc  0 181,1, 0  ** surface Canopy conductance [m/s]
CDsfc  0 252,1, 0  ** surface Surface drag coefficient [non-dim]
CDCONclm  0 72,200,0 ** atmos column Convective cloud cover [%]
CDLYRclm  0 213,200,0 ** atmos column Non-convective cloud [%]
CFRZRsfc  0 141,1, 0  ** surface Categorical freezing rain [yes=1;no=0]
CICEPsfc  0 142,1, 0  ** surface Categorical ice pellets [yes=1;no=0]
CINsfc  0 156,1, 0  ** surface Convective inhibition [J/kg]
CIN180_0mb  0 156,116,46080 ** 180-0 mb above gnd Convective inhibition [J/kg]
CLWMRprs 29 153,100,0 ** Cloud water [kg/kg]
CNWATsfc  0 223,1, 0  ** surface Plant canopy surface water [kg/m^2]
CRAINsfc  0 140,1, 0  ** surface Categorical rain [yes=1;no=0]
CSNOWsfc  0 143,1, 0  ** surface Categorical snow [yes=1;no=0]
DLWRFsfc  0 205,1, 0  ** surface Downward longwave radiation flux [W/m^2]
DPT2m  0 17,105,2 ** 2 m Dew point temp. [K]
DSWRFsfc  0 204,1, 0  ** surface Downward shortwave radiation flux [W/m^2]
EVPsfc  0 57,1, 0  ** surface Evaporation [kg/m^2]
FRICVsfc  0 253,1, 0  ** surface Surface friction velocity [m/s]
GFLUXsfc  0 155,1, 0  ** surface Ground Heat Flux [W/m^2]
HCDChcl  0 75,234,0 ** high cloud level High level cloud cover [%]
HGTprs 29 7,100,0 ** Geopotential height [gpm]
HGThbl  0 7,109,1  ** Geopotential height [gpm]
HGTclb  0 7,2,0 ** cloud base Geopotential height [gpm]
HGTclt  0 7,3,0 ** cloud top Geopotential height [gpm]
HGT0deg  0 7,4,0 ** 0C isotherm level Geopotential height [gpm]
HGTmwl  0 7,6,0 ** max wind level Geopotential height [gpm]
HGTtrp  0 7,7,0 ** tropopause Geopotential height [gpm]
HLCYhlr  0 190,106,7680  ** Storm relative helicity [m^2/s^2]
HPBLsfc  0 221,1, 0  ** surface Planetary boundary layer height [m]
ICMRprs 29 178,100,0 ** Ice mixing ratio [kg/kg]
LCDClcl  0 73,214,0 ** low cloud level Low level cloud cover [%]
LFTX500_1000mb 0 131,101,12900 ** 500-1000 mb Surface lifted index [K]
LHTFLsfc  0 121,1, 0  ** surface Latent heat flux [W/m^2]
MCDCmcl  0 74,224,0 ** mid-cloud level Mid level cloud cover [%]
MCONVprs  0 135,100,850  ** Horizontal moisture divergence [kg/kg/s]
MCONVhbl  0 135,109,1  ** Horizontal moisture divergence [kg/kg/s]
MCONV30_0mb  0 135,116,7680 ** 30-0 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV60_30mb  0 135,116,15390 ** 60-30 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV90_60mb  0 135,116,23100 ** 90-60 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV120_90mb  0 135,116,30810 ** 120-90 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV150_120mb  0 135,116,38520 ** 150-120 mb above gnd Horizontal moisture divergence [kg/kg/s]
MCONV180_150mb  0 135,116,46230 ** 180-150 mb above gnd Horizontal moisture divergence [kg/kg/s]
MSLETmsl  0 130,102,0  ** Mean sea level pressure (ETA model) [Pa]
MSTAVdlr  0 207,112,100  ** Moisture availability [%]
PEVAPsfc  0 228,1, 0  ** surface Potential evaporation [kg/m^2]
POTsfc  0 13,1, 0  ** surface Potential temp. [K]
POT10m  0 13,105,10 ** 10 m Potential temp. [K]
POT30m  0 13,105,30 ** 30 m Potential temp. [K]
POThbl  0 13,109,1  ** Potential temp. [K]
PRATEsfc  0 59,1, 0  ** surface Precipitation rate [kg/m^2/s]
PRESsfc  0 1,1, 0  ** surface Pressure [Pa]
PRES2m  0 1,105,2 ** 2 m Pressure [Pa]
PRES10m  0 1,105,10 ** 10 m Pressure [Pa]
PRES30m  0 1,105,30 ** 30 m Pressure [Pa]
PREShbl  0 1,109,1  ** Pressure [Pa]
PRESclb  0 1,2,0 ** cloud base Pressure [Pa]
PRESclt  0 1,3,0 ** cloud top Pressure [Pa]
PRESadcl  0 1,5,0 ** adiabatic lifting condensation level Pressure [Pa]
PRESmwl  0 1,6,0 ** max wind level Pressure [Pa]
PREStrp  0 1,7,0 ** tropopause Pressure [Pa]
PRESNsfc  0 134,1, 0  ** surface Pressure (nearest grid point) [Pa]
PRMSLmsl  0 2,102,0  ** Pressure reduced to MSL [Pa]
PWATclm  0 54,200,0 ** atmos column Precipitable water [kg/m^2]
RCQsfc  0 248,1, 0  ** surface Humidity parameter in canopy conductance [fraction]
RCSsfc  0 246,1, 0  ** surface Solar parameter in canopy conductance [fraction]
RCSOLsfc  0 249,1, 0  ** surface Soil moisture parameter in canopy conductance [fraction]
RCTsfc  0 247,1, 0  ** surface Temperature parameter in canopy conductance [fraction]
RH2m  0 52,105,2 ** 2 m Relative humidity [%]
RHhbl  0 52,109,1  ** Relative humidity [%]
RH0deg  0 52,4,0 ** 0C isotherm level Relative humidity [%]
SFEXCsfc  0 208,1, 0  ** surface Exchange coefficient [(kg/m^3)(m/s)]
SHTFLsfc  0 122,1, 0  ** surface Sensible heat flux [W/m^2]
SNODsfc  0 66,1, 0  ** surface Snow depth [m]
SNOHFsfc  0 229,1, 0  ** surface Snow phase-change heat flux [W/m^2]
SNOMsfc  0 99,1, 0  ** surface Snow melt [kg/m^2]
SNOWCsfc  0 238,1, 0  ** surface Snow cover [%]
SOILLSoilT  0 160,112,10 ** 0-10cm undergnd Liquid volumetric soil moisture (non-frozen) [fraction]
SOILLdlr 3 160,112,0 ** Liquid volumetric soil moisture (non-frozen) [fraction]
SOILMdlr  0 86,112,200  ** Soil moisture content [kg/m^2]
SOILWSoilT  0 144,112,10 ** 0-10cm undergnd Volumetric soil moisture (frozen + liquid) [fraction]
SOILWdlr 3 144,112,0 ** Volumetric soil moisture (frozen + liquid) [fraction]
SPFHprs 29 51,100,0 ** Specific humidity [kg/kg]
SPFH2m  0 51,105,2 ** 2 m Specific humidity [kg/kg]
SPFH10m  0 51,105,10 ** 10 m Specific humidity [kg/kg]
SPFH30m  0 51,105,30 ** 30 m Specific humidity [kg/kg]
SPFHhbl  0 51,109,1  ** Specific humidity [kg/kg]
SPFH30_0mb  0 51,116,7680 ** 30-0 mb above gnd Specific humidity [kg/kg]
SPFH60_30mb  0 51,116,15390 ** 60-30 mb above gnd Specific humidity [kg/kg]
SPFH90_60mb  0 51,116,23100 ** 90-60 mb above gnd Specific humidity [kg/kg]
SPFH120_90mb  0 51,116,30810 ** 120-90 mb above gnd Specific humidity [kg/kg]
SPFH150_120mb  0 51,116,38520 ** 150-120 mb above gnd Specific humidity [kg/kg]
SPFH180_150mb  0 51,116,46230 ** 180-150 mb above gnd Specific humidity [kg/kg]
SSRUNsfc  0 235,1, 0  ** surface Surface runoff (non-infiltrating) [kg/m^2]
TCDCclm  0 71,200,0 ** atmos column Total cloud cover [%]
TKEprs 15 158,100,0 ** Turbulent Kinetic Energy [J/kg]
TKEhbl  0 158,109,1  ** Turbulent Kinetic Energy [J/kg]
TMPsfc  0 11,1, 0  ** surface Temp. [K]
TMPprs 29 11,100,0 ** Temp. [K]
TMP2m  0 11,105,2 ** 2 m Temp. [K]
TMP10m  0 11,105,10 ** 10 m Temp. [K]
TMP30m  0 11,105,30 ** 30 m Temp. [K]
TMPhbl  0 11,109,1  ** Temp. [K]
TMP30_0mb  0 11,116,7680 ** 30-0 mb above gnd Temp. [K]
TMP60_30mb  0 11,116,15390 ** 60-30 mb above gnd Temp. [K]
TMP90_60mb  0 11,116,23100 ** 90-60 mb above gnd Temp. [K]
TMP120_90mb  0 11,116,30810 ** 120-90 mb above gnd Temp. [K]
TMP150_120mb  0 11,116,38520 ** 150-120 mb above gnd Temp. [K]
TMP180_150mb  0 11,116,46230 ** 180-150 mb above gnd Temp. [K]
TMPclt  0 11,3,0 ** cloud top Temp. [K]
TMPtrp  0 11,7,0 ** tropopause Temp. [K]
TSOILdpl  0 85,111,800  ** Soil temp. [K]
TSOILSoilT  0 85,112,10 ** 0-10cm undergnd Soil temp. [K]
TSOILdlr 3 85,112,0 ** Soil temp. [K]
UFLXsfc  0 124,1, 0  ** surface Zonal momentum flux [N/m^2]
UGRDprs 29 33,100,0 ** u wind [m/s]
UGRD10m  0 33,105,10 ** 10 m u wind [m/s]
UGRD30m  0 33,105,30 ** 30 m u wind [m/s]
UGRDhbl  0 33,109,1  ** u wind [m/s]
UGRD30_0mb  0 33,116,7680 ** 30-0 mb above gnd u wind [m/s]
UGRD60_30mb  0 33,116,15390 ** 60-30 mb above gnd u wind [m/s]
UGRD90_60mb  0 33,116,23100 ** 90-60 mb above gnd u wind [m/s]
UGRD120_90mb  0 33,116,30810 ** 120-90 mb above gnd u wind [m/s]
UGRD150_120mb  0 33,116,38520 ** 150-120 mb above gnd u wind [m/s]
UGRD180_150mb  0 33,116,46230 ** 180-150 mb above gnd u wind [m/s]
UGRDmwl  0 33,6,0 ** max wind level u wind [m/s]
UGRDtrp  0 33,7,0 ** tropopause u wind [m/s]
ULWRFsfc  0 212,1, 0  ** surface Upward long wave radiation flux [W/m^2]
ULWRFtoa  0 212,8,0 ** top of atmos Upward long wave radiation flux [W/m^2]
USTMhlr  0 196,106,15360  ** u-component of storm motion [m/s]
USWRFsfc  0 211,1, 0  ** surface Upward short wave radiation flux [W/m^2]
USWRFtoa  0 211,8,0 ** top of atmos Upward short wave radiation flux [W/m^2]
VEGsfc  0 87,1, 0  ** surface Vegetation [%]
VFLXsfc  0 125,1, 0  ** surface Meridional momentum flux [N/m^2]
VGRDprs 29 34,100,0 ** v wind [m/s]
VGRD10m  0 34,105,10 ** 10 m v wind [m/s]
VGRD30m  0 34,105,30 ** 30 m v wind [m/s]
VGRDhbl  0 34,109,1  ** v wind [m/s]
VGRD30_0mb  0 34,116,7680 ** 30-0 mb above gnd v wind [m/s]
VGRD60_30mb  0 34,116,15390 ** 60-30 mb above gnd v wind [m/s]
VGRD90_60mb  0 34,116,23100 ** 90-60 mb above gnd v wind [m/s]
VGRD120_90mb  0 34,116,30810 ** 120-90 mb above gnd v wind [m/s]
VGRD150_120mb  0 34,116,38520 ** 150-120 mb above gnd v wind [m/s]
VGRD180_150mb  0 34,116,46230 ** 180-150 mb above gnd v wind [m/s]
VGRDmwl  0 34,6,0 ** max wind level v wind [m/s]
VGRDtrp  0 34,7,0 ** tropopause v wind [m/s]
VISsfc  0 20,1, 0  ** surface Visibility [m]
VSTMhlr  0 197,106,15360  ** v-component of storm motion [m/s]
VVELprs 29 39,100,0 ** Pressure vertical velocity [Pa/s]
VVELhbl  0 39,109,1  ** Pressure vertical velocity [Pa/s]
VVEL30_0mb  0 39,116,7680 ** 30-0 mb above gnd Pressure vertical velocity [Pa/s]
VVEL60_30mb  0 39,116,15390 ** 60-30 mb above gnd Pressure vertical velocity [Pa/s]
VVEL90_60mb  0 39,116,23100 ** 90-60 mb above gnd Pressure vertical velocity [Pa/s]
VVEL120_90mb  0 39,116,30810 ** 120-90 mb above gnd Pressure vertical velocity [Pa/s]
VVEL150_120mb  0 39,116,38520 ** 150-120 mb above gnd Pressure vertical velocity [Pa/s]
VVEL180_150mb  0 39,116,46230 ** 180-150 mb above gnd Pressure vertical velocity [Pa/s]
VWSHtrp  0 136,7,0 ** tropopause Vertical speed shear [1/s]
WCCONVplr  0 241,101,70  ** Water condensate flux convergence (vertical int) [kg/m^2/s]
WCCONVclm  0 241,200,0 ** atmos column Water condensate flux convergence (vertical int) [kg/m^2/s]
WCINCplr  0 233,101,70  ** water condensate added by precip assimilaition [kg/m^2/s]
WCINCclm  0 233,200,0 ** atmos column water condensate added by precip assimilaition [kg/m^2/s]
WCUFLXplr  0 244,101,70  ** Water condensate zonal flux (vertical int) [kg/m/s]
WCUFLXclm  0 244,200,0 ** atmos column Water condensate zonal flux (vertical int) [kg/m/s]
WCVFLXplr  0 245,101,70  ** Water condensate meridional flux (vertical int) [kg/m/s]
WCVFLXclm  0 245,200,0 ** atmos column Water condensate meridional flux (vertical int) [kg/m/s]
WEASDsfc  0 65,1, 0  ** surface Accum. snow [kg/m^2]
WVCONVplr  0 237,101,70  ** Water vapor flux convergence (vertical int) [kg/m^2/s]
WVCONVclm  0 237,200,0 ** atmos column Water vapor flux convergence (vertical int) [kg/m^2/s]
WVINCplr  0 232,101,70  ** water vapor added by precip assimilation [kg/m^2/s]
WVINCclm  0 232,200,0 ** atmos column water vapor added by precip assimilation [kg/m^2/s]
WVUFLXplr  0 242,101,70  ** Water vapor zonal flux (vertical int)[kg/m/s]
WVUFLXclm  0 242,200,0 ** atmos column Water vapor zonal flux (vertical int)[kg/m/s]
WVVFLXplr  0 243,101,70  ** Water vapor meridional flux (vertical int) [kg/m/s]
WVVFLXclm  0 243,200,0 ** atmos column Water vapor meridional flux (vertical int) [kg/m/s]
ENDVARS
EOF

cat > flx.ctl << EOF
dset ${INPUTNARR}/flx06/flx.ft06.%y4%m2
index ${INPUTNARR}/flx06/flx.idx
undef 9.999E+20
title flx.ft06.198707,8,9
*  produced by grib2ctl v0.9.12.5p32a
dtype grib 98
options yrev template
xdef 192 linear 0.000000 1.875
ydef 94 levels
-88.542 -86.653 -84.753 -82.851 -80.947 -79.043 -77.139 -75.235 -73.331 -71.426
-69.522 -67.617 -65.713 -63.808 -61.903 -59.999 -58.094 -56.189 -54.285 -52.380
-50.475 -48.571 -46.666 -44.761 -42.856 -40.952 -39.047 -37.142 -35.238 -33.333
-31.428 -29.523 -27.619 -25.714 -23.809 -21.904 -20.000 -18.095 -16.190 -14.286
-12.381 -10.476  -8.571  -6.667  -4.762  -2.857  -0.952   0.952   2.857   4.762
  6.667   8.571  10.476  12.381  14.286  16.190  18.095  20.000  21.904  23.809
 25.714  27.619  29.523  31.428  33.333  35.238  37.142  39.047  40.952  42.856
 44.761  46.666  48.571  50.475  52.380  54.285  56.189  58.094  59.999  61.903
 63.808  65.713  67.617  69.522  71.426  73.331  75.235  77.139  79.043  80.947
 82.851  84.753  86.653  88.542
tdef 2 linear ${HR}Z${DAYM1}${mname}${YEARM1} 6hr
zdef 1 linear 1 1
vars 2
LHTFLsfc  0 121,1,0 ** surface Latent heat flux [W/m^2]
SHTFLsfc  0 122,1,0 ** surface Sensible heat flux [W/m^2]
ENDVARS
EOF

/usrx/local/grads/bin/gribmap -0 -i awip3203cp_00-18.ctl &
/usrx/local/grads/bin/gribmap -0 -i awip3203cp_03-21.ctl &
/usrx/local/grads/bin/gribmap -0 -i flx.ctl &
wait

/usrx/local/grads/bin/grads -blc "run ${NARR}/sorc/monitoring/rgtable4.gs"
${EXEC}/convert4.x
mv rrvsgr ${TMPNARR}/vsdb/eta_${DATM12}.vdb

if [ $CYC = 00 ]; then
#ls -l ${TMPNARR}/vsdb/eta_${YEARM1}${MONTHM1}${DAYM1}00.vdb 
#ls -l ${TMPNARR}/vsdb/eta_${YEARM1}${MONTHM1}${DAYM1}12.vdb
cat ${TMPNARR}/vsdb/eta_${YEARM1}${MONTHM1}${DAYM1}*vdb > ${TMPNARR}/vsdb/eta_${YEARM1}${MONTHM1}${DAYM1}.vsdb
rm -f ${TMPNARR}/vsdb/*vdb
fi

#==============================================================================
# transfer to HPSS
#==============================================================================
 sh ${NARR}/scr/run_ftp_restrt.sh ${STREAM} ${DATE}
 if [ $CYC = 00 ]; then
    DATE_M1=`$UTIL/ndate -24 $DATE`
    sh ${NARR}/scr/run_ftp_edas.sh ${STREAM} ${DATE_M1}
 fi
#==============================================================================

if [ $DAY -eq 01 -a $CYC -eq 00 ]; then
   sh ${NARR}/scr/eta_plot.sh ${STREAM} ${DATE}
fi

# Get ready for next cycle
DATE=`$UTIL/ndate +12 $DATE`

LASTDATE=`cut -c 1-10 ${INIDIR}/LASTDATE`

################################################################################
done  ######### while [ $DATE -le $DATEND ]; do
################################################################################

echo "$0 ENDDATE "`date`

exit 0
