#! /bin/sh
#-----------------------------------------------------
# script collect_gspath.sh 
# This shell script collects the Navy Gulf Stream  
# north/south wall file from the operational buffer 
# and puts it in a subdirectory under cfspom. 
# usage:    sh collect_gspath.sh  
# William O'Connor July 2003 
# last modified Jan. 2, 2004 to test for data subdirectory 
# and make it if not there
#---------------------------------------------------
set -x 
typeset -Z3 JD  
### For a manual run comment out the next two lines and
### then set the CALENDAR and JULIAN dates in the subsequent
### lines. 
## for today's calendar and julian dates 
PDY=`date +%Y%m%d`
JD=`date +%j`
## for previous calendar and julian dates   
##PDY=20030613
##JD=164
#####  should be no need to change lines below  
echo PDY ${PDY}
PDY00=${PDY}'00'
echo PDY00 ${PDY00}
echo 'JD=' ${JD}
#
YY=`echo $PDY | cut -c1-4`
MM=`echo $PDY | cut -c5-6`
DD=`echo $PDY | cut -c7-8`
YYMM=`echo $PDY | cut -c1-6`
# find yesterday's date 
YDATE=`/nwprod/util/exec/ndate -24 $PDY00`
echo YDATE ${YDATE} 
YESDATE=`echo $YDATE | cut -c1-8`
echo YESDATE ${YESDATE}
#LYYMM=`echo $YDATE | cut -c1-6`
#echo LYYMM ${LYYMM}
#
BUFFER_PDY='/dcom/us007003/'${PDY}'/wtxtbul'
echo 'BUFFER_PDY=' ${BUFFER_PDY}
BUFFER_YESDATE='/dcom/us007003/'${YESDATE}'/wtxtbul'
echo 'BUFFER_YESDATE=' ${BUFFER_YESDATE} 
DATA_DIR='/nfsuser/g01/cfspom/daily_data/gspath'
echo 'DATA_DIR=' ${DATA_DIR} 
cd ${DATA_DIR}
pwd
# make monthly subdirectory if it does not already exist    
if [ -d ${DATA_DIR}/${YYMM} ]
then
  echo 'directory ' ${DATA_DIR}/${YYMM} 'exists'
else
  mkdir ${DATA_DIR}/${YYMM}  
fi
#--------------------------------------------------------
# set name of Gulf Stream file to be copied 
FILE='gs'${JD}'nw.sub'  
echo 'FILE=' ${FILE} 
#
# copy the Gulf Stream file from production tanks
# try  yesterday's buffer first and then today's 
if [ -f ${BUFFER_YESDATE}/${FILE} ] 
then 
   rcp ${BUFFER_YESDATE}/${FILE}   ${DATA_DIR}/${YYMM}/${PDY}'.gspath' 
   echo 'Gulf Stream file' ${FILE} ' copied to ' ${PDY}'.gspath' 
   break 
elif [ -f ${BUFFER_PDY}/${FILE} ]
then 
   rcp ${BUFFER_PDY}/${FILE}       ${DATA_DIR}/${YYMM}/${PDY}'.gspath'  
   echo 'Gulf Stream file' ${FILE} ' copied to ' ${PDY}'.gspath' 
   break
else
   echo 'GULF STREAM FILE' ${FILE} ' NOT PRESENT'   
fi 

# check to see if file is there and change permission 
if [ -f ${DATA_DIR}/${YYMM}/${PDY}'.gspath' ]
then
   chmod 444  ${DATA_DIR}/${YYMM}/${PDY}'.gspath'  
fi
# end of script 
