#!/bin/sh

#set -x
export basecom=${basecom:-/u/Robert.Grumbine/noscrub/}

export tag=${PDYm1:-`date +"%Y%m%d"`}
export com=${com:-$basecom/com/}

export log=Robert.Grumbine@prodwcoss:
#export log=
#--------------------------------------------------------
if [ ! -d $com ] ; then
  mkdir -p $com
fi
if [ ! -d $com/gfs/prod ] ; then
  mkdir -p $com/gfs/prod
fi
cd $com/gfs/prod

if [ ! -d gdas.${tag} ] ; then
  mkdir gdas.${tag}
else 
  echo already have gdas.${tag}
  exit
fi

cd gdas.${tag}
#switch 12 may 2016
scp -p ${log}/com2/gfs/prod/gdas.$tag/gdas1.t00z.sfluxgrbf0[0-5] .
scp -p ${log}/com2/gfs/prod/gdas.$tag/gdas1.t06z.sfluxgrbf0[0-5] .
scp -p ${log}/com2/gfs/prod/gdas.$tag/gdas1.t12z.sfluxgrbf0[0-5] .
scp -p ${log}/com2/gfs/prod/gdas.$tag/gdas1.t18z.sfluxgrbf0[0-5] .
