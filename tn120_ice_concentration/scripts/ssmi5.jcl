#QSUB -o  ssmi.getout
#QSUB -lT 150
#QSUB -lM   8.0Mw
#QSUB -me

#Script for handling the production and dissemination of all ssmi-derived
#  sea ice concentration files
#Bob Grumbine 1 March 1995.

SDIR=/wd2/wd21/wd21rg/execs
ADIR=/ombptmp/ice/analy/fifteenth
ADIR2=/ombptmp/ice/analy/sixteenth
RDIR=/marine/ice/ice.analy
RDIR2=/data/forecasts/ice.analy
CDIR=/wd2/wd21/wd21rg/control

#Nothing below here should need changing on a new system

cd /tmp/wd21rg

tag=`date +'%y%m%d'`
jday=`date +'%j'`
if [ ! -f ssmi ] ; then
  cp ${SDIR}/ssmi .
fi

ja

# Get the SSMI orbit by orbit file
#fetch x -fBB -mVS -t"dsn=com.psatavsp.sharepr.ss5idb,disp=shr" 
#name change 3/15/95
if [ ! -s unblock.$tag ] ; then
  fetch x -fBB -mVS -t"dsn=com.spprod.ss5idb,disp=shr" 
  uscpblock -u -fBB x   unblock.$tag
  rm x
fi

# Run the analysis on the files
refyear=`date +'%Y' `
${SDIR}/ssmi unblock.$tag ${CDIR}/nland.map ${CDIR}/sland.map \
       nssmi.$tag sssmi.$tag a2north.$tag a2south.$tag a1 a2 $jday $refyear 

cp ?ssmi.$tag ${ADIR}
rcp ?ssmi.$tag cray4:${RDIR}
rcp ?ssmi.$tag polar:${RDIR2}
rcp a2???th.$tag cray4:${RDIR}
#remsh hp20 -l wd21rg ssmi/crayget

#
#
#Now get the NESDIS and Air Force maps for the northern hemisphere
#
#

#START GET NESDIS ICE MAP
#if [ ! -f newlo.$tag ] ; then
#  fetch newlo.$tag -mVS -fTB -t"dsn=nss.psadtivt.dmsp.mif11n5,disp=shr"
#name change 3/15/95?
#  fetch newlo.$tag -mVS -fTB -t"dsn=nss.psadtivt.dmsp.mif11n5,disp=shr"
#fi

#Extract selected fields
#assign -a newlo.$tag -Fcos -Nibm fort.20
#assign -a binar.$tag fort.52

#echo 4 > ain
#echo 1 >> ain
#echo 2 >> ain
#echo 4 >> ain
#echo 16 >> ain
#echo 2 >> ain
#echo 2 >> ain
#echo 3 >> ain
#${SDIR}/fields.512 < ain
#
##Process them to ice concentrations and write out compactly
#if [ ! -f ${SDIR}/icconc ] ; then
#  echo "Make the ice concentration converter"
#  exit
#fi
#${SDIR}/icconc binar.$tag nasa.$tag ssmi.$tag
#
#rm binar.$tag nasa.$tag newlo.$tag
#mv ssmi.$tag ${ADIR2}
#END GET NESDIS ICE MAP



#Now get the Air Force analysis
if [ ! -f newlo.$tag ] ; then
#name change 3/15/95
#  fetch afin.$tag -mVS -fTB -t"dsn=com.psnsnirt.snodeph.nhmamap,disp=shr"
  fetch afin.$tag -mVS -fTB -t"dsn=com.spprod.snodeph.nhmamap,disp=shr"
fi

#Extract selected fields
assign -a afin.$tag -Fcos -Nibm fort.20
assign -a afout.$tag -s unblocked fort.21

${SDIR}/af

rcp afout.$tag cray4:${RDIR}
rcp afout.$tag polar:${RDIR2}
mv afout.$tag ${ADIR2}
rm afin.$tag

ja -chlst
