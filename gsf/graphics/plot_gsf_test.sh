#! /bin/sh
# plot_gsf_test.sh 
# This script plots the analysed Gulf Stream and the Navy Gulf Stream
# with IDL on the ROFS output.  There are two inputs: 
# yyyymmdd = date of front finder analysis 
# gsdate   = date of Navy north wall file (also yyyymmdd format)  
# Usage: sh plot_gsf_test.sh yyyymmdd gsdate  
# Feb. 2005 William O'Connor 
#------------------------------------------------------------------
set -x 
home='/usr1/home/cfspom/GSF_TEST' 
work='/usr1/tmp/cfspom/GSF_TEST'  
rofs_prod='/usr1/home/wd21js/js_test/cofs' 
gs_analy='/usr1/home/wd21js/js_test/gsfinder/test' 
gs_direct='/usr1/home/wd21js/js_test/gsfinder/nout_files'
wgrib='/usr/local/grads/grads-1.8sl11/bin/wgrib' 
idl='/usr/local/rsi/idl_6.0/bin/idl'  
PRINTER='Pphaser5' 
#------------------------------------------------------------------
# set the dates
yyyymmdd=$1
echo 'yyyymmdd = ' ${yyyymmdd}   # date of front finder analysis
gsdate=$2
echo 'gsdate = '   ${gsdate}     # date of Navy north wall file  
#------------------------------------------------------------------
# make the working directory and change to it  
if [ -d ${work} ]
then
   echo ${work} 'exists'
else
   mkdir -p ${work} 
fi
cd ${work}
pwd  
#------------------------------------------------------------------
# remove any previous files from working directory
/bin/rm -f ${work}/*.n00
/bin/rm -f ${work}/*.std   
/bin/rm -f ${work}/*.txt
/bin/rm -f ${work}/*.exe 
/bin/rm -f ${work}/*.f90 
/bin/rm -f ${work}/*.out 
/bin/rm -f ${work}/*.err 
/bin/rm -f ${work}/nout.* 
/bin/rm -f ${work}/clean.* 
/bin/rm -f ${work}/*.pro  
/bin/rm -f ${work}/*.ps  
/bin/rm -f ${work}/idlfile 
#------------------------------------------------------------------
# copy the ROFS output average standard grib file to working directory
if [ -f ${rofs_prod}/${yyyymmdd}00n00.agb.std ] 
then 
     cp ${rofs_prod}/${yyyymmdd}00n00.agb.std  ${work}/ 
else
   echo "FILE ${rofs_prod}/${yyyymmdd}00n00.agb.std NOT PRESENT"
   echo 'SCRIPT WILL EXIT'    
   exit 1 
fi 
#-------------------------------------------------------------------
# copy the analyzed Gulf Stream file to the working directory
if [ -f ${gs_analy}/analyzed.${yyyymmdd}.n00 ] 
then 
     cp ${gs_analy}/analyzed.${yyyymmdd}.n00   ${work}/clean.${yyyymmdd} 
else
     echo "FILE ${gs_analy}/analyzed.${yyyymmdd}.n00 NOT PRESENT"
     echo 'SCRIPT WILL EXIT'
     exit 1 
fi  
#-------------------------------------------------------------------
# copy the Navy north wall file to the working directory 
if [ ${gs_direct}/nout.${gsdate} ]
then
   cp ${gs_direct}/nout.${gsdate}  ${work}/  
else 
   echo "FILE  ${gs_direct}/nout.${gsdate} NOT PRESENT"  
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
#-------------------------------------------------------------------
# degrib the average surface height field
${wgrib}  ${yyyymmdd}00n00.agb.std | grep ":DSLM:" |    \
          ${wgrib} -i -text ${yyyymmdd}00n00.agb.std -o wl.txt 
#
# degrib the average surface temperature field 
${wgrib}  ${yyyymmdd}00n00.agb.std | grep "kpds6=160:kpds7=0" | egrep ":WTMP:" |  \
          ${wgrib} -i -text ${yyyymmdd}00n00.agb.std -o WTMP.txt 
#
# degrib the average surface salinity field 
${wgrib}  ${yyyymmdd}00n00.agb.std | grep "kpds6=160:kpds7=0" | egrep ":SALTY:" |  \
          ${wgrib} -i -text ${yyyymmdd}00n00.agb.std -o SALTY.txt 
#
# degrib the average surface U-velocity field 
${wgrib}  ${yyyymmdd}00n00.agb.std | grep "kpds6=160:kpds7=0" | egrep ":UOGRD:" |  \
          ${wgrib} -i -text ${yyyymmdd}00n00.agb.std -o UOGRD.txt 
#
# degrib the average surface V-velocity field 
${wgrib}  ${yyyymmdd}00n00.agb.std | grep "kpds6=160:kpds7=0" | egrep ":VOGRD:" |  \
          ${wgrib} -i -text ${yyyymmdd}00n00.agb.std -o VOGRD.txt 
#
#---------------------------------------------------------------------
# copy the grib file reformatting program to working directory and compile
cp ${home}/read_standard.f90      ${work}/  
f90 -o ${work}/read_standard.exe  ${work}/read_standard.f90 
if [ -f ${work}/read_standard.exe ]
then
   echo "program ${work}/read_standard.f90  compiled" 
else
   echo "PROGRAM ${work}/read_standard.f90 DID NOT COMPILE"
   echo 'SCRIPT WILL EXIT' 
   exit 1
fi 
#-------------------------------------------------------------------------
# execute the grib file reformatting program 
${work}/read_standard.exe 1> ${work}/read_standard.out \
                          2> ${work}/read_standard.err    
#---------------------------------------------------------------------------
# copy the idl plotting program to working directory
cp ${home}/plot_standard.pro   ${work}/
#----------------------------------------------------------------------
# run IDL program to plot Gulf Stream on ROFS standard grid output
# creating .ps files 
echo ".run plot_standard.pro"               > idlfile
echo ps                                    >> idlfile
echo ${yyyymmdd}                           >> idlfile
wc -l clean.${yyyymmdd}                    >> idlfile
echo ${gsdate}                             >> idlfile   
wc -l nout.${gsdate}                       >> idlfile
echo "exit"                                >> idlfile 
${idl}                                      < idlfile

lpr -${PRINTER} < ${work}/plot_standard.ps  
#---------------------------------------------------------------------
# end 
