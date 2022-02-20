#!/bin/sh

set -x

tagm=20160813
tag=20160814
end=20160814
#--------------------------------------------------------------------------------------
echo zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz loading modules zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
module load prod_util grib_util
echo zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz done loading modules zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

# Bring the various environment-sensitive definitions out of J jobs and to here:

export HOMEbase=/u/Robert.Grumbine/rgdev
export seaice_concentration_analysis_ver=v4.2.0
export HOMEseaice_concentration_analysis=$HOMEbase/seaice_concentration_analysis.${seaice_concentration_analysis_ver}

export envir=dev2
export RUN=sice
export cyc=${cyc:-00}

if [ -z $obsproc_dump_ver ] ; then
  echo null obsproc_dump_ver
  export obsproc_dump_ver=v3.2.0
  export obsproc_shared_bufr_dumplist_ver=v1.1.0
fi


if [ "$envir" = 'prod' ] || [ "$envir" = 'para' ] || [ "$envir" = 'test' ] || [ "$envir" = 'dev' ]; then
  #NCO
  export NET=omb
  export HOMEbase=/nw${envir}
  export COMROOT=/com/${NET}/${envir}
  export COMINsst_base=/com/gfs/prod
  export PCOMROOT=/pcom/${envir}/mrf
else
  #developer
  export NET=mmab
  export HOMEbase=/u/Robert.Grumbine/rgdev
  export DCOMROOT=/dcom
  export COMROOT=/u/Robert.Grumbine/noscrub/com/${NET}/${envir}
  export PCOMROOT=/u/Robert.Grumbine/noscrub/pcom/$RUN
  #export COMINsst_base=/u/Robert.Grumbine/noscrub/com/gfs/prod
  export COMINsst_base=/com/gfs/prod
  export DATAROOT=/ptmpd2/wx21rg/
  export jlogfile=~/jlogfile
fi
export pid=$$


########################################################
# set up jlogfile
########################################################
if [ "$envir" = 'prod' ] || [ "$envir" = 'para' ] || [ "$envir" = 'test' ] || [ "$envir" = 'dev' ] ; then
  #NCO
  if [ "$envir" = 'prod' ] ; then
    export jlogfile=/com/logs/jlogfiles/jlogfile.${job}.${pid}
  else
    export jlogfile=/com/logs/${envir}/jlogfile
  fi
fi

########################################################
#DBN stuff
########################################################
export DBNROOT=/nwprod/spa_util/fakedbn
if [ "$envir" = "prod" ] ; then
  export SENDDBN=YES
  export SENDDBN_NTC=YES
else
  if [ $envir = "para" ] ; then
    export SENDDBN=YES
    export SENDDBN_NTC=NO
    export DBN_ALERT_TYPE=TBD_PARA
  else
    export SENDDBN=NO
    export SENDDBN_NTC=NO
  fi
fi
########################################################



#--------------------------------------------------------------------------------------
#The actual running of stuff

while [ $tag -le $end ]
do
  export PDY=$tag
  export PDYm1=$tagm

  export job=seaice_concentration_filter
  export DATA=$DATAROOT/${job}.${pid}
  mkdir $DATA
  time ./sms.filter.fake > sms.filter.$tag

  export job=seaice_concentration_analysis
  export DATA=$DATAROOT/${job}.${pid}
  mkdir $DATA
#Required for dumpjb to run:
  export TMPDIR=$DATA
  time ./sms.fake > sms.$tag

#  module load gempak
#  time ../jobs/JICE_GEMPAK > gempak.$tag

  tagm=$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
