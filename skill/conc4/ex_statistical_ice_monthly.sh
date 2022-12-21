#!/bin/sh

#Make statistical estimates based on 5 'models' of the monthly
#  mean sea ice extent.
#version a is to just extend the results of set up from parameters of
#  first 20 years of CFSR data
#version b is to update with all data through current month.
#given this structure, the 20 year fcsts are constant through 2100 (infinity)
#each month for 'all' data fcsts will be updated only in that month.
#
#'models' are constant, linear trend, quadratic trend, ensemble logistic
#
#overplot 'observed' values on the smooth regression curves.
#
#later: collect Wanqiu's 9 months 
#later: collect Xingren's re-examinations
#
#score predictions


cd running/directory

#Get most recent month of hires ice concs:
scp seaice@emcrzdm:/home/ftp/polar/cdas/archive/ice5min.${yy}${mm}.gz .
gunzip ice5min.${yy}${mm}.gz
wgrib ice5min.${yy}${mm} | wgrib -i -nh ice5min.${yy}${mm} -o ice.bin
$exec/areas ice.bin > icearea.${yy}${mm}

cp $fix/icearea.* .


#for the 20 year models, get the parameters and run up prediction to 2040
cp $fix/climoparms.$mm .
$exec/climo  climoparms.$mm 2040 > climopred

#for the 'all' models, compute and write out parameters, run up prediction to 2040
# ? maybe do a standard +30 years?
# icearea.*${mm} ?
$exec/allpred icearea.*$mm 2040 allout.$mm allparms.$mm 

#Later: run for multiple subregions




#Finished
cp icearea.${yy}$mm $fix
cp allparms $COMOUT
cp allout   $COMOUT
