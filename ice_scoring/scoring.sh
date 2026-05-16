#!/bin/sh

#Score ice edge from ice concentration analysis vs. the IMS analysis
#Robert Grumbine 21 May 2018

tag=${tag:-`date +"%Y%m%d"`}
#tag=20180517
export IMSDIR=/u/Robert.Grumbine/onoscrub/imssnow/
export NCEPANALY=/u/Robert.Grumbine/onoscrub/sice/
export RTOFSDIR=/u/Robert.Grumbine/onoscrub/rtofs_cice/com2/rtofs/prod/
export ACNFSDIR=/u/Robert.Grumbine/onoscrub/acnfs/
export EXDIR=/u/Robert.Grumbine/rgdev/mmablib/ice_scoring/ice_edge/C/
export RUNDIR=/ptmpd2/wx21rg/edging/

if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
fi
cd $RUNDIR
module load NetCDF

#Utility files -- distance to land, points to skip
if [ ! -f seaice_alldist.bin ] ; then
  cp /u/Robert.Grumbine/rgdev/iceanaly_nco/fix/seaice_alldist.bin .
fi
if [ ! -f skip ] ; then
  cp /u/Robert.Grumbine/rgdev/mmablib/ice_scoring/fix/skip_hr skip
fi 
if [ ! -f skip -o ! -f seaice_alldist.bin ] ; then
  echo cannot run  Missing skip and/or seaice_alldist.bin
  exit 1
fi

#Get the ncep analysis and ims analysis
if [ ! -f ${IMSDIR}/${tag}/wgrbbul/imssnow96.grb.grib2 ] ; then
  if [ ! -d ${IMSDIR}/${tag}/wgrbbul ] ; then
    mkdir -p ${IMSDIR}/${tag}/wgrbbul
  fi
  cp -rp /dcom/us007003/$tag/wgrbbul/imssnow96.grb.grib2  ${IMSDIR}/${tag}/wgrbbul/imssnow96.grb.grib2
fi
if [ ! -f ${IMSDIR}/${tag}/wgrbbul/imssnow96.grb.grib2 ] ; then
  echo Could not find ims analysis for $tag, aborting
  exit 1
fi

if [ ! -f ${NCEPANALY}/${tag}/seaice.t00z.5min.grb.grib2 ] ; then
  if [ ! -d ${NCEPANALY}/${tag} ] ; then
    mkdir -p ${NCEPANALY}/${tag}
  fi
  cp -p /com2/omb/prod/sice.$tag/seaice.t00z.5min.grb.grib2 ${NCEPANALY}/${tag}/seaice.t00z.5min.grb.grib2
fi
if [ ! -f ${NCEPANALY}/${tag}/seaice.t00z.5min.grb.grib2 ] ; then
  echo Could not find ncep analysis for $tag, aborting
  exit 1
fi

# Get the model -- cice output from RTOFS
if [ ! -d ${RTOFSDIR}/rtofs.$tag ] ; then
#hh in n-24, n00, f24, f48, f72, 96, 120, 144, 168, 192
  mkdir $tag
  cp /com2/rtofs/prod/rtofs.$tag/rtofs_glo.t00z.*.cice_inst $tag
else
  ln -sf ${RTOFSDIR}/rtofs.$tag $tag
fi

#--- Starting to work locally --===============================================
if [ ! -f ncepanaly.$tag ] ; then
  wgrib2 ${NCEPANALY}/${tag}/seaice.t00z.5min.grb.grib2 | wgrib2 -i ${NCEPANALY}/${tag}/seaice.t00z.5min.grb.grib2 -no_header -order we:ns -bin ncepanaly.$tag
fi
if [ ! -f imsanaly.$tag ] ; then
  wgrib2 ${IMSDIR}/${tag}/wgrbbul/imssnow96.grb.grib2 | wgrib2 -i ${IMSDIR}/${tag}/wgrbbul/imssnow96.grb.grib2 -no_header -bin imsanaly.$tag
  if [ ! -f imsanaly.$tag ] ; then
    echo Could not construct imsanaly.$tag
  fi
fi


# IMS edge -- concentration independent
if [ ! -f imsanaly.${tag}.edge ] ; then
  ${EXDIR}/find_edge_ims  imsanaly.$tag  > imsanaly.${tag}.edge
fi


# CICE and ncepanaly edges depend on concentration:
for t in 15 30 35 40 45 50 55 
do 
  if [ ! -f ncepanaly${t}.${tag}.edge ] ; then
    time ${EXDIR}/find_edge      ncepanaly.$tag  seaice_alldist.bin 0.$t > ncepanaly${t}.${tag}.edge
  fi

  hh=n00
  if [ ! -f ciceanaly${t}.${hh}.${tag}.edge ] ; then
    time ${EXDIR}/find_edge_cice skip $tag/rtofs_glo.t00z.${hh}.cice_inst 0.$t > ciceanaly${t}.${hh}.${tag}.edge
  fi
done

#Now do the actual scoring
for t in 15 30 35 40 45 50 55 
do
  if [ ! -f ai.${t}.$tag -a -f imsanaly.${tag}.edge ] ; then
    time ${EXDIR}/cscore_edge ncepanaly${t}.${tag}.edge imsanaly.${tag}.edge      > ai.${t}.$tag 
  fi
  if [ ! -f ia.${t}.$tag ] ; then
    time ${EXDIR}/cscore_edge imsanaly.${tag}.edge      ncepanaly${t}.${tag}.edge > ia.${t}.$tag 
  fi
done

for t in 15 30 35 40 45 50 55 
do
  hh=n00
  if [ ! -f ci.${t}.$hh.$tag -a -f imsanaly.${tag}.edge ] ; then
    time ${EXDIR}/cscore_edge ciceanaly${t}.${hh}.${tag}.edge imsanaly.${tag}.edge      > ci.${t}.$hh.$tag
  fi
  if [ ! -f ic.${t}.$hh.$tag -a -f imsanaly.${tag}.edge ] ; then
    time ${EXDIR}/cscore_edge imsanaly.${tag}.edge      ciceanaly${t}.${hh}.${tag}.edge > ic.${t}.$hh.$tag
  fi
  if [ ! -f ca.${t}.$hh.$tag ] ; then
    time ${EXDIR}/cscore_edge ciceanaly${t}.${hh}.${tag}.edge ncepanaly${t}.${tag}.edge  > ca.${t}.$hh.$tag
  fi
  if [ ! -f ac.${t}.$hh.$tag ] ; then
    time ${EXDIR}/cscore_edge ncepanaly${t}.${tag}.edge  ciceanaly${t}.${hh}.${tag}.edge > ac.${t}.$hh.$tag
  fi

done
