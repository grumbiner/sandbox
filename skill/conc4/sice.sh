#!/bin/sh

export basecom=${basecom:-/u/Robert.Grumbine/noscrub/}

#-------------------------------------------------------------------
tag=${tag:-`date +"%Y%m%d"`}
export com=${com:-$basecom/com/}

if [ ! -d $com/mmab/prod ] ; then
  mkdir -p $com/mmab/prod
fi
cd $com/mmab/prod/

if [ ! -d sice.${tag} ] ; then
  mkdir sice.${tag}
  cd sice.$tag
  scp -p Robert.Grumbine@prodwcoss:/com2/omb/prod/sice.$tag/seaice.* .
else 
  if [ ! -f sice.${tag}/seaice.t00z.5min.grb ] ; then
    cd sice.$tag
    scp -p Robert.Grumbine@prodwcoss:/com2/omb/prod/sice.$tag/seaice.* .
  fi
fi

