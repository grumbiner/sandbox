#! /bin/sh
# /data/WWW/cofs/COFS2/developmental/main/archive_COFS2.sh 
# This schell script looks for data in the ROFS2 website
# directory older than 6 days, archives the .png files,
# and then deletes the subdirectory
# Oct 2004  William O'Connor 
# ------------------------------------------------------
set -x 
home='/data/WWW/cofs/COFS2/developmental/main' 
NDATE='/usr1/cfspom/ndate/ndate'  
archive_dir='/usr1/cfspom/ARCHIVE'  
#-------------------------------------------------------
cd ${home}
pwd 
#------------------------------------------------------
# get the dates  
# for today 
PDY=`date +"%Y%m%d"` 
echo 'PDY = ' ${PDY} 
#
# for 6 days ago 
PDYm6=`echo -144 ${PDY}00 | ${NDATE} `  
PDYm6=`echo ${PDYm6} | cut -c1-8`  
echo 'PDYm6 = ' ${PDYm6}  
#
# for 7 days ago 
PDYm7=`echo -168 ${PDY}00 | ${NDATE} `  
PDYm7=`echo ${PDYm7} | cut -c1-8`  
echo 'PDYm7 = ' ${PDYm7}  
#
# for 8 days ago 
PDYm8=`echo -192 ${PDY}00 | ${NDATE} `  
PDYm8=`echo ${PDYm8} | cut -c1-8`  
echo 'PDYm8 = ' ${PDYm8}  
#
# for 9 days ago 
PDYm9=`echo -216 ${PDY}00 | ${NDATE} `  
PDYm9=`echo ${PDYm9} | cut -c1-8`  
echo 'PDYm9 = ' ${PDYm9}  
#-----------------------------------------------------
# Test to see directories from 6 to 9 days old are present.
# If so, copy graphic files to home directory archive and then delete.  
# 
for day in ${PDYm6} ${PDYm7} ${PDYm8} ${PDYm9}
do 
# 
if [ -d ${home}/${day} ]
then
   echo "directory ${home}/${day} present"
   /bin/rm -f ${home}/${day}/first.xml 
   /bin/rm -f ${home}/${day}/welcome.xsl 
   yyyy=`echo ${day} | cut -c1-4`
#   echo 'yyyy = ' ${yyyy} 
#   ls -al ${home}/${day}/${yyyy}'_'*/*.png 
   mkdir ${archive_dir}/${day}    
# copy the .png files to home directory daily archive 
   cp ${home}/${day}/${yyyy}'_'*/*.png  ${archive_dir}/${day}  
# remove files and directories 
   /bin/rm -f ${home}/${day}/*.html
   /bin/rm -f ${home}/${day}/*.xsl 
   /bin/rm -f ${home}/${day}/*.xml 
   /bin/rm -f ${home}/${day}/${yyyy}'_'*/*.png
   /bin/rm -f ${home}/${day}/${yyyy}'_'*/*.xsl
   /bin/rm -f ${home}/${day}/${yyyy}'_'*/*.xml 
   /bin/rm -f ${home}/${day}/${yyyy}'_'*/*.html 
   rmdir      ${home}/${day}/${yyyy}'_'*/ 
   rmdir      ${home}/${day}/ 
else
   echo "DIRECTORY ${home}/${day} NOT PRESENT"
fi 

done 
#------------------------------------------------------
# end 
