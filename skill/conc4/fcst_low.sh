#!/bin/sh

export PDY=${tag:-`date +"%Y%m%d"`}

export nwprod=${nwprod:-/u/Robert.Grumbine/desk/model}
export com=${com:-/u/Robert.Grumbine/noscrub}

export FIX=${nwprod}/statistical/fix
export EXEC=${nwprod}/statistical/exec

export COMINbase=${com}/com/mmab/prod/
export COMOUTbase=${com}/com/mmab/developer/

export BASEDIR=/usr2/rmg3/wx21rg
export BASEDIR=/ptmpd2/wx21rg/stats

PATH=$PATH:/usr/local/grads/bin:.

#----------------------------------------------------
job=stats
pid=$$
export DATA=$BASEDIR/${job}.$pid
mkdir -p $DATA
cd $DATA

set -xe
#----------------------------------------------------
ln -sf $FIX/skip_hr skip

if [ -f ${COMINbase}/sice.$PDY/seaice.t00z.5min.grb ] ; then
  wgrib ${COMINbase}/sice.$PDY/seaice.t00z.5min.grb | wgrib -i -nh ${COMINbase}/sice.$PDY/seaice.t00z.5min.grb -o conc.$PDY
else
  echo no analysis file for $PDY
  exit
fi
  
  if [ -s conc.$PDY ] ; then
    $EXEC/fcst_low conc.$PDY skip fcst.$PDY
  else
    echo error, could not get a file for the analyzed ice concentration
    exit
  fi

  if [ ! -d $COMOUTbase/stats.$PDY ] ; then
    mkdir -p $COMOUTbase/stats.$PDY
  fi
  if [ -s fcst.$PDY ] ; then
    cp fcst.$PDY $COMOUTbase/stats.$PDY
  fi
