#! /bin/sh
# /u/wx21rg/rofs_gsf/save/run_gsf_graphics.sh 
# This script copies and reads the daily output files of 
# the operational ROFS model. 
# The script gets the Front Finder Gulf Stream files and
# the Navy Gulf Stream files, and uses IDL graphics to  
# overplot Gulf Stream positions on ROFS ouput. 
# The script then converts .ps to .gif files and puts  
# them on the web for the Front Finder page. 
# usage:   sh run_gsf_graphics.sh 
# Jan. 2004  William O'Connor 
# Jan 2007 -- modified by Robert Grumbine to run on DEW
#-----------------------------------------------------------
set -x
set -e
# specify the directories 
DATADIR='/com/cofs/prod' 
GSPROD='/com/omb/prod' 
SORC='/u/wx21rg/save/rofs_gsf/graphics'
WKDIR='/stmp/wx21rg/gsf'
idldir='/u/wx21rg/save/rofs_gsf/graphics/idl' 
gs_direct='/gpfs/d/marine/save/cfspom/daily_data/gspath/NOUT_DIR'
NDATE='/nwprod/util/exec/ndate'  
PATH=$PATH:/u/wx21rg/bin:/u/wx21rg/bin/netpbm:$WKDIR
gif_remote_base='/data/WWW/gsf'  
ftp_remote_base='/var/ftp/pub/gsf'  
remote_host='polar.ncep.noaa.gov' 
remote_login='rmg3'
SCP='/usr/bin/scp'  
#------------------------------------------------------------------------------------
export PATH WKDIR SORC 
#Generate the graphics using the scripting from Dave Michaud for path names
export IMPATH=/usrx/local/imajik
export LIBPATH=$LIBPATH:$IMPATH/lib
export DELEGATE_PATH=$IMPATH/share/ImageMagick
#########################################
# Ghostscript
#########################################
export GSPATH=/usrx/local/ghostscript/gs7.05
export GS_LIB=$GSPATH
export GS_FONTPATH=$GSPATH/fonts
export GS_INIT=$GSPATH/gs_init.ps
export GS_LIB_DEFAULT=$GSPATH:$GS_FONTPATH

export PATH=$IMPATH/bin:$GSPATH:$PATH
#------------------------------------------------------------
# set the dates
tdate=`date '+%Y%m%d'00`
echo 'todays date tdate ' ${tdate}
ndate=`${NDATE} -24 ${tdate}` 
echo 'nowcast date ndate ' ${ndate} 
fdate=`${NDATE} +24 ${tdate}` 
echo '48 hr forecast date ' ${fdate} 
YMD=`echo ${tdate} | cut -c1-8`
echo 'YMD ' ${YMD} 
YMDp1=`${NDATE} +24 ${YMD}00 | cut -c1-8` 
echo 'YMDp1 = '  ${YMDp1}
YMDp2=`${NDATE} +24 ${YMDp1}00 | cut -c1-8` 
echo 'YMDp2 = '  ${YMDp2}
YMDm1=`${NDATE} -24 ${YMD}00 | cut -c1-8`
echo 'YMDm1 = ' ${YMDm1}  
YMDm2=`${NDATE} -24 ${YMDm1}00 | cut -c1-8`
echo 'YMDm2 = ' ${YMDm2}  
YMDm3=`${NDATE} -24 ${YMDm2}00 | cut -c1-8`
echo 'YMDm3 = ' ${YMDm3}  
YMDm4=`${NDATE} -24 ${YMDm3}00 | cut -c1-8`
echo 'YMDm4 = ' ${YMDm4}  
YMDm5=`${NDATE} -24 ${YMDm4}00 | cut -c1-8`
echo 'YMDm5 = ' ${YMDm5}  
YMDm6=`${NDATE} -24 ${YMDm5}00 | cut -c1-8`
echo 'YMDm6 = ' ${YMDm6}  
YMDm7=`${NDATE} -24 ${YMDm6}00 | cut -c1-8`
echo 'YMDm7 = ' ${YMDm7}  
YMDm8=`${NDATE} -24 ${YMDm7}00 | cut -c1-8`
echo 'YMDm8 = ' ${YMDm8}  
YMDm9=`${NDATE} -24 ${YMDm8}00 | cut -c1-8`
echo 'YMDm9 = ' ${YMDm9}  
YMDm10=`${NDATE} -24 ${YMDm9}00 | cut -c1-8`
echo 'YMDm10 = ' ${YMDm10}  
#-------------------------------------------------------------
# make working directory if necessary and change to it  
if [ -d ${WKDIR} ]
then
   echo ${WKDIR} 'exists'
else
   mkdir -p ${WKDIR} 
fi 
cd ${WKDIR}
pwd 
#-------------------------------------------------------------
# remove any previous files 
/bin/rm -f ${WKDIR}/*.igb  
/bin/rm -f ${WKDIR}/*.ps 
/bin/rm -f ${WKDIR}/*.pro
/bin/rm -f ${WKDIR}/*.tplate 
/bin/rm -f ${WKDIR}/*.txt 
/bin/rm -f ${WKDIR}/*.idl 
/bin/rm -f ${WKDIR}/idlfile3  
/bin/rm -f ${WKDIR}/*.dat
/bin/rm -f ${WKDIR}/*.gif
/bin/rm -f ${WKDIR}/nout.*  
/bin/rm -f ${WKDIR}/clean.*  
#--------------------------------------------------------------
# Find the most recent Navy north wall file and copy it to the
# working directory
if [ -f ${gs_direct}/nout.${YMD} ]
then
   scp ${gs_direct}/nout.${YMD}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMD} 
   gsdate=${YMD}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm1} ]
then
   scp ${gs_direct}/nout.${YMDm1}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm1} 
   gsdate=${YMDm1}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm2} ]
then
   scp ${gs_direct}/nout.${YMDm2}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm2} 
   gsdate=${YMDm2}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm3} ]
then
   scp ${gs_direct}/nout.${YMDm3}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm3} 
   gsdate=${YMDm3}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm4} ]
then
   scp ${gs_direct}/nout.${YMDm4}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm4} 
   gsdate=${YMDm4}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm5} ]
then
   scp ${gs_direct}/nout.${YMDm5}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm5} 
   gsdate=${YMDm5}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm6} ]
then
   scp ${gs_direct}/nout.${YMDm6}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm6} 
   gsdate=${YMDm6}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm7} ]
then
   scp ${gs_direct}/nout.${YMDm7}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm7} 
   gsdate=${YMDm7}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm8} ]
then
   scp ${gs_direct}/nout.${YMDm8}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm8} 
   gsdate=${YMDm8}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm9} ]
then
   scp ${gs_direct}/nout.${YMDm9}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm9} 
   gsdate=${YMDm9}  
   break 
elif [ -f ${gs_direct}/nout.${YMDm10} ]
then
   scp ${gs_direct}/nout.${YMDm10}        ${WKDIR}/nout.hold 
   echo 'Copied Navy file ' ${gs_direct}/nout.${YMDm10} 
   gsdate=${YMDm10}  
   break 
else
   echo 'NO NAVY FILE FOUND IN PAST 10 DAYS '
   echo 'WILL CREATE A FICTITIOUS NAVY FILE THAT WILL NOT BE PLOTTED'  
   echo '00.0 00.0' >  ${WKDIR}/nout.hold 
   gsdate='00000000' 
fi 
#_________________________________________________________________
# reformat file so first line is date and second line is 
# number of data lines to be read
echo ${gsdate}                       > ${WKDIR}/nout.navy   
wc -l            ${WKDIR}/nout.hold >> ${WKDIR}/nout.navy  
cat              ${WKDIR}/nout.hold >> ${WKDIR}/nout.navy 
#--------------------------------------------------------------
# copy the IDL template files to working directory 
scp ${idldir}/find_max_mask.pro        ${WKDIR}
scp ${idldir}/find_min_mask.pro        ${WKDIR}
scp ${idldir}/plot-cofs_elev.tplate    ${WKDIR} 
#-----------------------------------------------------------------
# copy bathymetry file to working directory
scp /u/wx21rg/save/rofs_gsf/graphics/util/ecofs-bathy.dat    ${WKDIR} 
#---------------------------------------------------------------
# copy the ROFS NOWCAST production output grib files to working directory 
if [ -f ${DATADIR}/cofs.${YMD}/cofs.t00z.n00.igb ] 
then
    scp ${DATADIR}/cofs.${YMD}/cofs.t00z.n00.igb      ${WKDIR} 
else 
    echo ${DATADIR}/cofs.${YMD}/cofs.t00z.n00.igb 'NOT COPIED' 
    exit 1
fi 
#
# copy the GSFINDER Gulf Stream nowcast file to production directory
scp -p ${GSPROD}/gsf.${YMD}/analyzed.${YMD}.n00  ${WKDIR}/clean.hold 
# reformat file so first line is date and second line is 
# number of data lines to be read
echo ${YMD}                           > ${WKDIR}/clean.out   
wc -l            ${WKDIR}/clean.hold >> ${WKDIR}/clean.out  
cat              ${WKDIR}/clean.hold >> ${WKDIR}/clean.out 
#
# process nowcast data and make plots 
mv ${WKDIR}/cofs.t00z.n00.igb  ${WKDIR}/${ndate}'n.igb' 

sh ${SORC}/degrib.sh ${ndate}'n.igb' SIGMA 1  nowcast YES 00   
#sh ${SORC}/degrib.sh ${ndate}'n.igb' SIGMA 18 nowcast YES 00
#sh ${SORC}/degrib.sh ${ndate}'n.igb' Z     50 nowcast YES 00
#sh ${SORC}/degrib.sh ${ndate}'n.igb' Z    100 nowcast YES 00
#sh ${SORC}/degrib.sh ${ndate}'n.igb' Z    150 nowcast YES 00
#sh ${SORC}/degrib.sh ${ndate}'n.igb' Z    200 nowcast YES 00

rm ${WKDIR}/clean.out 
rm ${WKDIR}/clean.hold 
#----------------------------------------------------------------
# copy the ROFS 24-h FORECAST production output grib files to working directory 
#
if [ -f ${DATADIR}/cofs.${YMD}/cofs.t00z.f24.igb ] 
then 
   scp ${DATADIR}/cofs.${YMD}/cofs.t00z.f24.igb      ${WKDIR} 
else 
   echo ${DATADIR}/cofs.${YMD}/cofs.t00z.f24.igb 'NOT COPIED' 
   exit 1 
fi 
#
# copy the GSFINDER Gulf Stream 24 h forecast file to production directory
scp -p ${GSPROD}/gsf.${YMD}/analyzed.${YMD}.f24  ${WKDIR}/clean.hold
# reformat file so first line is date and second line is 
# number of data lines to be read
echo ${YMDp1}                         > ${WKDIR}/clean.out   
wc -l            ${WKDIR}/clean.hold >> ${WKDIR}/clean.out  
cat              ${WKDIR}/clean.hold >> ${WKDIR}/clean.out 
#
# process the 24 hr forecast data and make plots 
mv ${WKDIR}/cofs.t00z.f24.igb  ${WKDIR}/${tdate}'f24.igb' 

sh ${SORC}/degrib.sh ${tdate}'f24.igb' SIGMA 1  forecast YES 24
#sh ${SORC}/degrib.sh ${tdate}'f24.igb' SIGMA 18 forecast YES 24
#sh ${SORC}/degrib.sh ${tdate}'f24.igb' Z     50 forecast YES 24
#sh ${SORC}/degrib.sh ${tdate}'f24.igb' Z    100 forecast YES 24
#sh ${SORC}/degrib.sh ${tdate}'f24.igb' Z    150 forecast YES 24
#sh ${SORC}/degrib.sh ${tdate}'f24.igb' Z    200 forecast YES 24

rm ${WKDIR}/clean.out 
rm ${WKDIR}/clean.hold 
#---------------------------------------------------------------

# copy the ROFS 48-h FORECAST production output grib files to working directory 
#
if [ -f ${DATADIR}/cofs.${YMD}/cofs.t00z.f48.igb ] 
then
   scp ${DATADIR}/cofs.${YMD}/cofs.t00z.f48.igb      ${WKDIR} 
else
   echo ${DATADIR}/cofs.${YMD}/cofs.t00z.f48.igb 'NOT COPIED' 
   exit 1
fi 
#
# copy the GSFINDER Gulf Stream 48 h forecast file to production directory
scp -p ${GSPROD}/gsf.${YMD}/analyzed.${YMD}.f48  ${WKDIR}/clean.hold  
# reformat file so first line is date and second line is 
# number of data lines to be read
echo ${YMDp2}                         > ${WKDIR}/clean.out   
wc -l            ${WKDIR}/clean.hold >> ${WKDIR}/clean.out  
cat              ${WKDIR}/clean.hold >> ${WKDIR}/clean.out 
#
# process the 48 hr forecast data and make plots 
mv ${WKDIR}/cofs.t00z.f48.igb  ${WKDIR}/${fdate}'f48.igb' 

sh ${SORC}/degrib.sh ${fdate}'f48.igb' SIGMA  1 forecast YES 48
#sh ${SORC}/degrib.sh ${fdate}'f48.igb' SIGMA 18 forecast YES 48
#sh ${SORC}/degrib.sh ${fdate}'f48.igb' Z     50 forecast YES 48
#sh ${SORC}/degrib.sh ${fdate}'f48.igb' Z    100 forecast YES 48
#sh ${SORC}/degrib.sh ${fdate}'f48.igb' Z    150 forecast YES 48
#sh ${SORC}/degrib.sh ${fdate}'f48.igb' Z    200 forecast YES 48

rm ${WKDIR}/clean.out 
rm ${WKDIR}/clean.hold 
#---------------------------------------------------------------
# Convert postscript files (.ps) into IGF files (.gif) 
  cd ${WKDIR}
  for psfile in `ls *.ps`
  do 
    nf=`basename $psfile .ps`
    convert -rotate -90 $psfile $nf.gif
  done
#-------------------------------------------------------------------
# rename the files before transfer to remote host 
mv ${WKDIR}/${YMD}'00-DSLM-1n.gif'   ${WKDIR}/n003.gif   
mv ${WKDIR}/${YMDp1}'00-DSLM-1f.gif' ${WKDIR}/f243.gif   
mv ${WKDIR}/${YMDp2}'00-DSLM-1f.gif' ${WKDIR}/f483.gif   
#---------------------------------------------------------------
# copy the gif files to web directory on remote host (polar) 
${SCP} -p ${WKDIR}/*.gif  ${remote_login}@${remote_host}:${gif_remote_base}  
# copy the analyzed Gulf Stream north wall files to ftp directory on polar
${SCP} -p ${GSPROD}/gsf.${YMD}/analyzed.${YMD}.*  ${remote_login}@${remote_host}:${ftp_remote_base}    
#---------------------------------------------------------------

exit
# copy files to remote host rzdm
remote_host='rzdm.ncep.noaa.gov'
remote_login=seaice
gif_remote_base='/home/people/emc/www/htdocs/mmab/gsf' 
ftp_remote_base='/home/people/emc/ftp/mmab/gsf' 
# copy the gif files to web directory on remote host (rzdm) 
# ${SCP} -p ${WKDIR}/*.gif  ${remote_login}@${remote_host}:${gif_remote_base}  

${SCP} -p ${WKDIR}/n003.gif  ${remote_login}@${remote_host}:${gif_remote_base}  
${SCP} -p ${WKDIR}/f243.gif  ${remote_login}@${remote_host}:${gif_remote_base}  
${SCP} -p ${WKDIR}/f483.gif  ${remote_login}@${remote_host}:${gif_remote_base}  


# copy the analyzed Gulf Stream north wall files to ftp directory on rzdm 
${SCP} -p ${GSPROD}/gsf.${YMD}/analyzed.${YMD}.*  ${remote_login}@${remote_host}:${ftp_remote_base}    
#---------------------------------------------------------------
# end 
