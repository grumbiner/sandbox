#!/bin/sh

export DCOMROOT=/u/Robert.Grumbine/onoscrub/amsr2/dcom
export obsproc_dump_ver=v4.0.0
export obsproc_shared_bufr_dumplist_ver=v1.4.0
echo dcom $DCOM dcomroot $DCOMROOT

export working=/ptmpd2/wx21rg/april
export output=/u/Robert.Grumbine/onoscrub/amsr2
mkdir -p $working
cd $working
if [ $? -ne 0 ] ; then
  echo could not cd to $working, exiting
  exit
fi


tag=20170401
while [ $tag -le 20170430 ]
do 
echo working on $tag

if [ ! -f ${output}/amsr2.$tag ] ; then
  export TMPDIR=/ptmpd2/wx21rg/tmpamsr2.$$
  time /nwprod2/obsproc_dump.v4.0.0/ush/dumpjb ${tag}00 12 amsr2 > dumpout.$tag

  ln -sf amsr2.ibm fort.11
  time /u/Robert.Grumbine/rgdev/seaice_analysis.v4.3.0/exec/seaice_amsrbufr > out.$tag
  err=$?
  if [ $err -ne 0 ] ; then
    echo error $err exiting now
    exit
  fi
  mv fort.52 ${output}/amsr2.$tag
  rm amsr2.ibm
fi

tag=`expr $tag + 1`
tag=`dtgfix3 $tag`

done
