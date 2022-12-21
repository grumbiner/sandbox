#! /bin/sh
#-----------------------------------------------------
# /nfsuser/g01/cfspom/daily_data/gspath/get_gspath.sh 
# This shell script searches the 10 days of buffer
# directories for all Navy Gulf Stream  
# north/south wall files it can find.  
# The julian date of the Navy file is not associated with
# the buffer calendar date.  A fortran program reads the 
# Navy file to obtain the calendar date.  Each Navy file
# is parsed, and copied to storage directories.   
# usage:    sh get_gspath.sh  
# March 2004    William O'Connor 
#---------------------------------------------------
set -x 
#---------------------------------------------------------
# set the pathways 
wkdir='/gpfstmp/cfspom/GSPATH'
NDATE='/nwprod/util/exec/ndate' 
sorc='/nfsuser/g01/cfspom/daily_data/gspath'  
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
# set the Julian Days for today and back 9 days. 
# The julian date of the file will not be associated
# with the calendar date of the buffer 
jd=`date +%j`
typeset -Z3 jd
echo 'jd   = ' ${jd}  
jdm1=`expr ${jd} - 1`
typeset -Z3 jdm1
echo 'jdm1 = ' ${jdm1} 
jdm2=`expr ${jdm1} - 1`
typeset -Z3 jdm2
echo 'jdm2 = ' ${jdm2} 
jdm3=`expr ${jdm2} - 1`
typeset -Z3 jdm3
echo 'jdm3 = ' ${jdm3} 
jdm4=`expr ${jdm3} - 1`
typeset -Z3 jdm4
echo 'jdm4 = ' ${jdm4} 
jdm5=`expr ${jdm4} - 1`
typeset -Z3 jdm5
echo 'jdm5 = ' ${jdm5} 
jdm6=`expr ${jdm5} - 1`
typeset -Z3 jdm6
echo 'jdm6 = ' ${jdm6} 
jdm7=`expr ${jdm6} - 1`
typeset -Z3 jdm7
echo 'jdm7 = ' ${jdm7} 
jdm8=`expr ${jdm7} - 1`
typeset -Z3 jdm8
echo 'jdm8 = ' ${jdm8} 
jdm9=`expr ${jdm8} - 1`
typeset -Z3 jdm9
echo 'jdm9 = ' ${jdm9} 
#-------------------------------------------------------------
# Convert any negative Julian dates to days at end of year.
# Assume leap year 366 days, but this does not matter because
# buffer day and Gulf stream day are not related.  Also, the
# calendar day of the Gulf Stream file will be read from the
# file by fortran program read_gsdate.x and stored as shell
# variable date_tag   
if [ ${jdm1} -le 0 ] ; then jdm1=`expr ${jdm1} + 366` ; fi 
if [ ${jdm2} -le 0 ] ; then jdm2=`expr ${jdm2} + 366` ; fi 
if [ ${jdm3} -le 0 ] ; then jdm3=`expr ${jdm3} + 366` ; fi 
if [ ${jdm4} -le 0 ] ; then jdm4=`expr ${jdm4} + 366` ; fi 
if [ ${jdm5} -le 0 ] ; then jdm5=`expr ${jdm5} + 366` ; fi 
if [ ${jdm6} -le 0 ] ; then jdm6=`expr ${jdm6} + 366` ; fi 
if [ ${jdm7} -le 0 ] ; then jdm7=`expr ${jdm7} + 366` ; fi 
if [ ${jdm8} -le 0 ] ; then jdm8=`expr ${jdm8} + 366` ; fi 
if [ ${jdm9} -le 0 ] ; then jdm9=`expr ${jdm9} + 366` ; fi 
#-------------------------------------------------------------
# create working directory and change to it 
if [ -d ${wkdir} ] 
then
   echo ${wkdir} 'exists'
else
   mkdir -p ${wkdir}
fi
cd ${wkdir}
pwd  
#-----------------------------------------------------------
# remove any previous files 
/bin/rm -f  ${wkdir}/gs*sub  
/bin/rm -f  ${wkdir}/nout*   
/bin/rm -f  ${wkdir}/sout*
/bin/rm -f  ${wkdir}/gsf_parse*  
/bin/rm -f  ${wkdir}/*.x 
/bin/rm -f  ${wkdir}/*.err   
/bin/rm -f  ${wkdir}/*.out 
#-----------------------------------------------------------
# copy the parse file and executable to working directory 
rcp ${sorc}/gsf_parse.pl    ${wkdir}  
rcp ${sorc}/read_gsdate.x   ${wkdir}  
rcp ${sorc}/read_path.exe   ${wkdir}  
#---------------------------------------------------------
export XLFRTEOPTS="unit_vars=yes"  
#---------------------------------------------------------
# This outer loop searches the 10 buffer directories 
for adate in ${YMD} ${YMDm1} ${YMDm2} ${YMDm3} ${YMDm4}  \
           ${YMDm5} ${YMDm6} ${YMDm7} ${YMDm8} ${YMDm9} 
do 

# This inner loop searches each daily buffer directory 
# for 10 possible julian date files. 
# Note that julian date of file is not associated with the
# calendar date of buffer.  Calendar date will be read from
# the file by fortran program read_gsdate.x. 
   for julia in ${jd} ${jdm1} ${jdm2} ${jdm3} ${jdm4}  \
              ${jdm5} ${jdm6} ${jdm7} ${jdm8} ${jdm9}
   do
     file_name=gs${julia}nw.sub  
     if [ -f "/dcom/us007003/${adate}/wtxtbul/${file_name}" ] 
     then
# copy Gulf Stream file to working directory
         rcp  "/dcom/us007003/${adate}/wtxtbul/${file_name}" ${wkdir} 
# read the date in the Gulf Stream file and write to file unit 90  
         export XLFUNIT_10="${file_name}" 
         export XLFUNIT_90="${file_name}.out" 
         ${wkdir}/read_gsdate.x 1> ${wkdir}/read_gsdate.out  
                                2> ${wkdir}/read_gsdate.err  
# read the date from file and store as shell variable 'date_tag' 
         cat ${file_name}.out | read date_tag
# parse the file 
         ${wkdir}/gsf_parse.pl nout.${date_tag} sout.${date_tag}  < ${file_name}  
# get year and month of file
         yyyymm=`echo ${date_tag} | cut -c1-6` 
         echo 'yyyymm = ' ${yyyymm}  
#
# make monthly subdirectory if it does not already exist    
         if [ -d ${sorc}/${yyyymm} ]
         then
              echo 'directory ' ${sorc}/${yyyymm} 'exists'
         else
              mkdir ${sorc}/${yyyymm}  
         fi
#
# copy files to be saved
         rcp ${wkdir}/${file_name}     ${sorc}/${yyyymm}/${date_tag}.gspath
         rcp ${wkdir}/${file_name}     ${sorc}/NOUT_DIR
         rcp ${wkdir}/nout.${date_tag} ${sorc}/NOUT_DIR
         rcp ${wkdir}/sout.${date_tag} ${sorc}/NOUT_DIR
         rcp ${sorc}/${yyyymm}/${date_tag}.gspath ${date_tag}.gspath
         echo ${date_tag} | $sorc/read_path.exe 
         rcp ${date_tag}.gspath2 ${sorc}/${yyyymm}/${date_tag}.gspath2

# remove files before calculation for next file 
         /bin/rm -f ${wkdir}/*.out
         /bin/rm -f ${wkdir}/nout*  
         /bin/rm -f ${wkdir}/sout* 
         /bin/rm -f ${wkdir}/${file_name}
     fi       
   done               # end of inner loop for Gulf Stream files  

done                  # end of outer loop for directories 
#----------------------------------------------------------
# end of script 
