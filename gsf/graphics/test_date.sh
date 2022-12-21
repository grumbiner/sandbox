#! /bin/sh
# /nfsuser/g01/cfspom/daily_data/gspath/test_date.sh
# This script takes a julian date as input, and 
# computes the past 9 julian days.  If it goes back 
# to the previous year it assumes a leap year.
# This is used only to test the proceedure for getting
# the Gulf Stream files, where it is not necessary to
# decide if it is a leap year.  
# usage:   sh test_date.sh  JD  
# William O'Connor 
#-----------------------------------------------------
set -x 
#---------------------------------------------------------
NDATE='/nwprod/util/exec/ndate' 
#---------------------------------------------------------
#
# set the Julian Days for today and back 9 days. 
# The julian date of the file will not be associated
# with the calendar date of the buffer 
# jd=`date +%j`
jd=$1
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
# end 
