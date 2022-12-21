#! /bin/sh
# /nfsuser/g01/cfspom/daily_data/gspath/push_gspath.sh 
# This script checks to see if latest Navy Gulf Stream
# north/south wall file is present in ROFS directory, 
# and pushes it to the specified computer (now stommel).
# Usage:  sh push_gspath.sh  computer
# April 2004  William O'Connor
#-------------------------------------------------------
set -x 
#-------------------------------------------------------
# get the input computer and determine dependent pathways 
computer=$1
echo 'computer = ' ${computer}  
case ${computer} in 
   stommel) 
     echo 'will scure copy to stommel'
     files_remote_base='/usr1/ofs/cfspom/gspath' 
     remote_host='stommel.ncep.noaa.gov'
     remote_login='cfspom'
     ;;
   *)
     echo 'SPECIFIED COMPUTER NOT IN LIST'  
     echo 'SCRIPT WILL EXIT'
     exit 1
     ;;
esac 
#
echo 'files_remote_base = ' ${files_remote_base}
echo 'remote_host =       ' ${remote_host}
echo 'remote_login =      ' ${remote_login}  
#---------------------------------------------------------
# set the fixed pathways
gsfile_base='/nfsuser/g01/cfspom/daily_data/gspath'  
NDATE='/nwprod/util/exec/ndate'  
SCP='/usr/bin/scp'  
#---------------------------------------------------------
# Set the calendar dates for today and back 9 days 
YMD=`date +%Y%m%d`
echo 'YMD = ' ${YMD} 
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
#-----------------------------------------------------
# go back over previous 6 days and copy any Navy
# Gulf Stream north/south wall files 
# /nfsuser/g01/cfspom/daily_data/gspath/YYYYMM/YYYYMMDD.gspath
# to remote computer
#
for yyyymmdd in ${YMD} ${YMDm1} ${YMDm2} ${YMDm3} \
                ${YMDm4} ${YMDm5} ${YMDm6}   
do
   echo 'yyyymmdd = ' ${yyyymmdd} 
   yyyymm=`echo ${yyyymmdd} | cut -c1-6`
   echo 'yyyymm = ' ${yyyymm}
   local_file=${gsfile_base}/${yyyymm}/${yyyymmdd}.gspath 
   echo 'local_file = ' ${local_file} 
   remote_dir=${files_remote_base}/${yyyymm}
   echo 'remote_dir = ' ${remote_dir}
#
   if [ -f ${local_file} ]
   then
      echo ${yyyymmdd}.gspath 'file present'
#     make directory on remote computer
      ssh -l ${remote_login} ${remote_host} mkdir ${remote_dir}        
#     secure copy to remote directory 
      ${SCP} -p ${local_file} ${remote_login}@${remote_host}:${remote_dir} 
   else
      echo ${yyyymmdd}.gspath 'FILE NOT PRESENT'
   fi   
done 

# end 
