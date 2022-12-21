#!/bin/sh

export basecom=${basecom:-/u/Robert.Grumbine/noscrub}

export tag=${tag:-`date +"%Y%m%d"`}
export com=${com:-$basecom/com/}

export log=Robert.Grumbine@prodwcoss:
#export log=

#-----------------------------------------------------
if [ ! -d ${com}/gfs/prod ] ; then
  mkdir -p ${com}/gfs/prod
fi
cd ${com}/gfs/prod

#Cleanup step:
#find . -type f -mmin +4330 -exec rm {} \; ; t2; dclean

if [ ! -d gfs.${tag} ] ; then
  mkdir gfs.${tag}
else 
  echo already have gfs.${tag}
  exit
fi

cd gfs.$tag
#switch to com2 12 may 2016
scp -p ${log}/com2/gfs/prod/gfs.$tag/gfs.t00z.sfluxgrbf06 .
scp -p ${log}/com2/gfs/prod/gfs.$tag/gfs.t00z.sfluxgrbf1[28] .
scp -p ${log}/com2/gfs/prod/gfs.$tag/gfs.t00z.sfluxgrbf[2-9][02468] .
scp -p ${log}/com2/gfs/prod/gfs.$tag/gfs.t00z.sfluxgrbf1?[02468] .
