#!/bin/ksh 

#Run where preaveraging has already been done

set -xe

export FIXsice=~/rgdev/drift/fix/
export EXECsice=~/rgdev/drift/exec/
export PDY=20220825
export COMINice_analy=~/rgdev/drift/bigfix/
export pgmout=OUTPUT.a
export HH=00

ln -sf seaice_forecast.points fort.47
ln -sf seaice_quote fort.91
ln -sf seaice_edge.t00z.txt fort.48

echo $PDY > alpha
ln -sf alpha     fort.90


set -x

#-------------------------- loop over each member for forecast
for mem in gep01 gep02 gep03 gep04 gep05 gep06 gep07 gep08 gep09 gep10 gep11 \
           gep12 gep13 gep14 gep15 gep16 gep17 gep18 gep19 gep20
do
  ln -sf u.averaged.${mem}.$PDY fort.11
  ln -sf v.averaged.${mem}.$PDY fort.12

  #-----------------------------------------------------
  #execute the model
  #-----------------------------------------------------
  time echo 32 | $EXECsice/seaice_sicedrft >> $pgmout 2>> errfile
  
  # Move each output file to temporary location:
  mv grid_ds grid_ds.$mem
  mv fort.60 fort.60.$mem
  mv fort.61 fort.61.$mem
  mv fort.62 fort.62.$mem
  mv fort.63 fort.63.$mem
  mv fort.64 fort.64.$mem
  # Will produce single best-guess kml file
  rm *.kml

done
echo done with running ensemble members

#-----------------------------------------------------
#NEW (2 June 2014) Down average to best guess from ensemble
#-----------------------------------------------------
#Blend the ensemble members down to a best guess
# Choked with large number of points 2022
time $EXECsice/seaice_midpoints fort.60.* fl.out ak.out >> $pgmout 2>> errfile

#Reformat for distribution
rm *.kml
#ln -sf fl.out fort.31
cp fl.out fort.31
msg="pgm seaice_reformat has begun"
time $EXECsice/seaice_reformat  >> $pgmout 2>> errfile


#copy to old names:
ln -sf fort.60 fl.out
ln -sf fort.61 ops.out
ln -sf fort.62 ak.out
ln -sf fort.63 global.tran
ln -sf fort.64 alaska.tran

