#! /bin/sh 
# standard_plot.sh 
# This script produces IDL graphics of the Gulf Stream Front 
# Finder output over the ROFS output on the standard grid.
# You must specify the date of Gulf Stream File below. 
# usage:   sh standard_plot.sh 
# Jan. 2005  William O'Connor 
#-------------------------------------------------------------
set -x 
# specify the paths 
HOME='/nfsuser/g01/cfspom/GSFINDER2'
WKDIR='/gpfstmp/cfspom/GSFINDER2' 
PROD='/com/cofs/prod' 
GSPROD='/com/omb/prod' 
gs_direct='/nfsuser/g01/cfspom/daily_data/gspath/NOUT_DIR'  
IDL='/usrx/local/rsi/idl_5.5/bin/idl' 
 PRINTER='Pphaser4' 
#PRINTER='Pphaser5' 
WGRIB='/nwprod/util/exec/wgrib' 
NDATE='/nwprod/util/exec/ndate'  
#-------------------------------------------------------------
# set the dates
PDY=`date +%Y%m%d`
echo 'PDY ' ${PDY}  
PDYp1=`${NDATE} +24 ${PDY}00 | cut -c1-8`
echo 'PDYp1 ' ${PDYp1} 
PDYp2=`${NDATE} +24 ${PDYp1}00 | cut -c1-8`
echo 'PDYp2 ' ${PDYp2} 

PDYm1=`${NDATE} -24 ${PDY}00 | cut -c1-8`
echo 'PDYm1 = ' ${PDYm1}  
PDYm2=`${NDATE} -24 ${PDYm1}00 | cut -c1-8`
echo 'PDYm2 = ' ${PDYm2}  
PDYm3=`${NDATE} -24 ${PDYm2}00 | cut -c1-8`
echo 'PDYm3 = ' ${PDYm3}  
PDYm4=`${NDATE} -24 ${PDYm3}00 | cut -c1-8`
echo 'PDYm4 = ' ${PDYm4}  
PDYm5=`${NDATE} -24 ${PDYm4}00 | cut -c1-8`
echo 'PDYm5 = ' ${PDYm5}  
PDYm6=`${NDATE} -24 ${PDYm5}00 | cut -c1-8`
echo 'PDYm6 = ' ${PDYm6}  
PDYm7=`${NDATE} -24 ${PDYm6}00 | cut -c1-8`
echo 'PDYm7 = ' ${PDYm7}  
PDYm8=`${NDATE} -24 ${PDYm7}00 | cut -c1-8`
echo 'PDYm8 = ' ${PDYm8}  
PDYm9=`${NDATE} -24 ${PDYm8}00 | cut -c1-8`
echo 'PDYm9 = ' ${PDYm9}  
PDYm10=`${NDATE} -24 ${PDYm9}00 | cut -c1-8`
echo 'PDYm10 = ' ${PDYm10}  
#--------------------------------------------------------------
# make working directory and change to it
if [ -d ${WKDIR} ]
then
   echo ${WKDIR} 'exists'
else
   mkdir -p ${WKDIR}
fi  
cd ${WKDIR}
pwd 
#------------------------------------------------------
# delete any old files in working directory
/bin/rm -f ${WKDIR}/cofs.t00z.*.igb.std
/bin/rm -f ${WKDIR}/*.txt
/bin/rm -f ${WKDIR}/*.exe 
/bin/rm -f ${WKDIR}/*.err
/bin/rm -f ${WKDIR}/*.out  
/bin/rm -f ${WKDIR}/*.f    
/bin/rm -f ${WKDIR}/*.pro
/bin/rm -f ${WKDIR}/idlfile 
/bin/rm -f ${WKDIR}/*.ps 
/bin/rm -f ${WKDIR}/clean.*
/bin/rm -f ${WKDIR}/nout.* 
#-----------------------------------------------------
# Find the most recent Navy north wall file and copy it to the
# working directory
if [ -f ${gs_direct}/nout.${PDY} ]
then
   rcp ${gs_direct}/nout.${PDY}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDY} 
   gsdate=${PDY}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm1} ]
then
   rcp ${gs_direct}/nout.${PDYm1}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm1} 
   gsdate=${PDYm1}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm2} ]
then
   rcp ${gs_direct}/nout.${PDYm2}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm2} 
   gsdate=${PDYm2}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm3} ]
then
   rcp ${gs_direct}/nout.${PDYm3}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm3} 
   gsdate=${PDYm3}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm4} ]
then
   rcp ${gs_direct}/nout.${PDYm4}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm4} 
   gsdate=${PDYm4}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm5} ]
then
   rcp ${gs_direct}/nout.${PDYm5}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm5} 
   gsdate=${PDYm5}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm6} ]
then
   rcp ${gs_direct}/nout.${PDYm6}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm6} 
   gsdate=${PDYm6}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm7} ]
then
   rcp ${gs_direct}/nout.${PDYm7}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm7} 
   gsdate=${PDYm7}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm8} ]
then
   rcp ${gs_direct}/nout.${PDYm8}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm8} 
   gsdate=${PDYm8}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm9} ]
then
   rcp ${gs_direct}/nout.${PDYm9}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm9} 
   gsdate=${PDYm9}  
   break 
elif [ -f ${gs_direct}/nout.${PDYm10} ]
then
   rcp ${gs_direct}/nout.${PDYm10}        ${WKDIR}/ 
   echo 'Copied Navy file ' ${gs_direct}/nout.${PDYm10} 
   gsdate=${PDYm10}  
   break 
else
   echo 'NO NAVY FILE FOUND IN PAST 10 DAYS '
   echo 'WILL CREATE A FICTITIOUS NAVY FILE THAT WILL NOT BE PLOTTED'  
   echo '00.0 00.0' >  ${WKDIR}/nout.${PDY} 
   gsdate=${PDY}
fi 
#
echo 'gsdate = ' ${gsdate}  
#-----------------------------------------------------
# copy the source code from home directory and compile
rcp ${HOME}/read_standard.f   ${WKDIR}
xlf90 -o read_standard.exe read_standard.f 
if [ -f  read_standard.exe ]
then
   echo 'program compiled'
else
   echo 'PROGRAM DID NOT COMPILE - EXIT'
   exit 1
fi 
#-----------------------------------------------------
# copy the IDL program to working directory 
rcp ${HOME}/plot_standard.pro   ${WKDIR}  
#-----------------------------------------------------
# This for loop obtains the ROFS ouptut, degrib the files, 
# and plots the output with IDL 
 for interval in n00 f24 f48 
#for interval in n00  
do 
# determine the valid dates
if [ ${interval} = 'n00' ] ; then validdate=${PDY} ; fi 
if [ ${interval} = 'f24' ] ; then validdate=${PDYp1} ; fi 
if [ ${interval} = 'f48' ] ; then validdate=${PDYp2} ; fi 
echo 'interval  = ' ${interval}
echo 'validdate = ' ${validdate} 
#-----------------------------------------------------
# copy the ROFS output files from production directory  
file=cofs.t00z.${interval}.igb.std 
file2=cofs.t00z.${interval}.agb.std 
rcp ${PROD}/cofs.${PDY}/${file}  ${WKDIR}
rcp ${PROD}/cofs.${PDY}/${file2}  ${WKDIR}
#
if [ -f ${WKDIR}/${file} ]
then 
   echo ${file} 'file copied'
else
   echo ${file} 'FILE NOT COPIED - EXIT'
   exit 1
fi 
#
if [ -f ${WKDIR}/${file2} ]
then 
   echo ${file2} 'file copied'
else
   echo ${file2} 'FILE NOT COPIED - EXIT'
   exit 1
fi 
#---------------------------------------------------------
# Degrib the file
# This inner for loop degribs the file to extract the  
# U-velocity, V-velocity, temperature, and salinity  
for parm in UOGRD VOGRD WTMP SALTY 
do 
${WGRIB} ${file} | grep "kpds6=160:kpds7=0" | egrep ${parm} \
        | ${WGRIB} -i -text ${file} -o ${parm}.txt  
done 
# 
# degrib the sea level to a file wl.txt 
#${WGRIB} ${file} | grep ":DSLM:" | ${WGRIB} -i -text ${file} -o wl.txt 
#
# degrib the average sea level to a file wl.txt 
${WGRIB} ${file2} | grep ":DSLM:" | ${WGRIB} -i -text ${file2} -o wl.txt 



# execute the fortran program to read these four files and 
# reformat them to one file uvst.txt 
${WKDIR}/read_standard.exe 1> ${WKDIR}/read_standard.out   \
                           2> ${WKDIR}/read_standard.err  
#-------------------------------------------------------------------
# copy the Gulf Stream path file from MMAB (OMB) production
gsfile=analyzed.${PDY}.${interval}  
echo ${gsfile} 
#
if [ -f   ${GSPROD}/gsf.${PDY}/${gsfile} ]
then 
   echo   ${GSPROD}/gsf.${PDY}/${gsfile} 'file present'
   rcp -p ${GSPROD}/gsf.${PDY}/${gsfile}   ${WKDIR}/clean.${validdate} 
else
   echo   ${GSPROD}/gsf.${PDY}/${gsfile} 'FILE NOT PRESENT - EXIT'
   exit 1
fi 
#----------------------------------------------------------------------
# run IDL program to plot Gulf Stream on ROFS standard grid output
# creating .ps files 
echo ".run plot_standard.pro"               > idlfile
echo ps                          >> idlfile
echo ${validdate}                >> idlfile
wc -l clean.${validdate}         >> idlfile
echo ${gsdate}                   >> idlfile   
wc -l nout.${gsdate}          >> idlfile
echo "exit"                      >> idlfile 
${IDL}         < idlfile

mv ${WKDIR}/plot_standard.ps  ${WKDIR}/plot${interval}.ps  
#  lpr -${PRINTER} < ${WKDIR}/plot${interval}.ps  

#---------------------------------------------------------------------
# remove files before making next plot 
rm -f ${WKDIR}/cofs.t00z.*.igb.std
rm -f ${WKDIR}/*.txt
rm -f ${WKDIR}/*.err
rm -f ${WKDIR}/*.out  
rm -f ${WKDIR}/idlfile 
rm -f ${WKDIR}/clean.*
#rm -f ${WKDIR}/*.ps  
#----------------------------------------------------------------------
done 
#
# end 
