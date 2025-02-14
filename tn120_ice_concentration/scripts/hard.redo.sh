#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/batchout
#@ error=/nfsuser/g01/wx21rg/ssmi/errout
#@ class=1
#@ wall_clock_limit=00:20:00
#@ queue

set -x

RUNDIR=/gpfstmp/wd21rg/ssmi
TMPDIR=$RUNDIR
EXDIR=/nfsuser/g01/wx21rg/oper/execs
DATDIR=/nfsuser/g01/wx21rg/ssmi/fix
OUTDIR=/emc1/ice/ice.analy
ADIR1=$OUTDIR
PATH=$PATH:/nwprod/util/exec/:/nfsuser/g01/marine/local/bin:/nfsuser/g01/marine/local/bin/netpbm
export RUNDIR TMPDIR EXDIR DATDIR OUTDIR ADIR1

if [ ! -d $RUNDIR ] ; then
  echo about to try to make directory $RUNDIR
  mkdir -p $RUNDIR
fi
cd $RUNDIR

tag4=${yy}$rest
tag4=20020114
tagm=`expr $tag4 - 1`
tagm=`/nfsuser/g01/marine/local/bin/dtgfix3 $tagm`
echo $tagm


jday=`date +"%j" `
echo Timing the dumpscript
time /nwprod/ush/dumpjb ${yy}${rest}00 12 ssmit
 
if [ -f fort.51 ] ; then
  rm fort.51
fi
if [ -f fort.14 ] ; then
  rm fort.14
fi
ln ssmit.ibm fort.14
time $EXDIR/ssmibufr.x 

echo fort.51 > delta
time $EXDIR/seaissmi.x delta $DATDIR/nland.map $DATDIR/sland.map \
  n3ssmi.$tag4.15 s3ssmi.$tag4.15 umasknorth.$tag4.15 umasksouth.$tag4.15 \
  b3north.$tag4.15 b3south.$tag4.15 $jday $yy 248 > v.15
time $EXDIR/seaissmi.x delta $DATDIR/nland.map $DATDIR/sland.map \
  n3ssmi.$tag4.14 s3ssmi.$tag4.14 umasknorth.$tag4.14 umasksouth.$tag4.14 \
  b3north.$tag4.14 b3south.$tag4.14 $jday $yy 247 > v.14
time $EXDIR/seaissmi.x delta $DATDIR/nland.map $DATDIR/sland.map \
  n3ssmi.$tag4.13 s3ssmi.$tag4.13 umasknorth.$tag4.13 umasksouth.$tag4.13 \
  b3north.$tag4.13 b3south.$tag4.13 $jday $yy 246 > v.13

#Make up the grib files for the polar stereographic data
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin

#Go through for each satellite and process to xpm and to grib:
for s in 13 14 15
do
  $EXDIR/rtxpm umasknorth.$tag4.${s} $DATDIR/nland.map nh${s}.$tag4.xpm umasksouth.$tag4.${s} $DATDIR/sland.map sh${s}.$tag4.xpm

  for pole in north south
  do
    if [ -f fort.11 ] ; then
       rm fort.11  
    fi
    if [ -f fort.51 ] ; then
       rm fort.51
    fi
    ln umask${pole}.$tag4.$s fort.11
    ${EXDIR}/psg${pole}.x < psin
    mv fort.51 ${pole}psg.$tag4.$s
    mv fort.52 wmo${pole}psg.j990l
  done
done 

#Transfer files to output directory:
cp *${tag4} *${tag4}.?? *${tag4}*xpm wmo* $OUTDIR

#Now begin the global file processing:

#QC denotes whether or not it will be possible to produce the
#  SST-quality controlled sea ice grids for modellers.

set -x

qc=false
if [ -s /com/fnl/prod/fnl.$tag4/gdas1.t00z.sstgrb ] ; then
  cp /com/fnl/prod/fnl.$tag4/gdas1.t00z.sstgrb oned
  qc=true
else
  j=$tag4
  while [ -d /com/fnl/prod/fnl.$j -a qc="false" ]
  do
    j=`expr $j - 1`
    j=`/nfsuser/g01/marine/local/bin/dtgfix3 $j`
    if [ -s /com/fnl/prod/fnl.$j/gdas1.t00z.sstgrb ] ; then
      cp /com/fnl/prod/fnl.$j/gdas1.t00z.sstgrb oned
      qc=true
    fi
  done
fi

echo qc flag is $qc

if [ $qc = "true" ] ; then
  #Filter the ice concentration field (in lat-long space only so far)
  if [ -f sst ] ; then
    rm sst
  fi
  wgrib oned | grep TMP | wgrib -i oned -o sst -nh -bin
  if [ ! -s sst ] ; then
    echo failed to get an sst field!
    exit 1
  fi
#NOTE: we are currently using only F-13 file.  This could be generalized.
  $EXDIR/filtanal.x sst umasknorth.${tag4}.13 umasksouth.${tag4}.13 \
        latlon.$tag4 nps.$tag4 sps.$tag4 $DATDIR/newland 275.3 \
        $DATDIR/nland.map $DATDIR/sland.map

  #fill in the ice concentration field with older data as required
  $EXDIR/icegrid.x $ADIR1/fill.$tagm latlon.$tag4 $ADIR1/age.$tagm \
                           age.$tag4 fill.$tag4
  retcode=$?

  if [ $retcode -ne 0 ] ; then
    if [ $retcode -eq 1 ] ; then
      echo Failed to open an input file
    elif [ $retcode -eq 2 ] ; then
      echo Failed to open an output file
    fi
  fi


  #Engrib the lat-long file
  echo $tag4 > ein
  ln -f fill.$tag4   fort.11
  touch fort.51
  ln -f fort.51      eng.$tag4
  $EXDIR/ice2grib.x < ein
  cp eng.$tag4    ${ADIR1}
  cp fort.52      ${ADIR1}/wmoglobice.990l

  #Remove the temporaries
  if [ -s latlon.$tag4 ] ; then
    cp latlon.$tag4 $ADIR1
  fi
  if [ -s age.$tag4 ] ; then
    cp age.$tag4 $ADIR1
  fi
  if [ -s fill.$tag4 ] ; then
    cp fill.$tag4 $ADIR1
  fi
fi #end of producing qc'd files

##########################################################
#
#Ensure that we don't provide excess permissions on files:
#
chmod a-x eng.* ???thpsg.* umask* fill.* *.gif
#
# Copy results over to polar, test directory
#
RBASEW=rmg3@polar.wwb.noaa.gov:/data/WWW/seaice
RBASEF=rmg3@polar.wwb.noaa.gov:/home/ftp/pub
rcp eng.$tag4             ${RBASEF}/cdas
rcp northpsg.${tag4}.13   ${RBASEF}/ice/northpsg.${tag4}
rcp southpsg.${tag4}.13   ${RBASEF}/ice/southpsg.${tag4}
rcp umasksouth.${tag4}.13 ${RBASEW}/analysis/south
rcp umasknorth.${tag4}.13 ${RBASEW}/analysis/north
rcp fill.$tag4            ${RBASEW}/analysis/filled
rcp fill.$tag4            ${RBASEW}/expt.analy/filled
rcp nh13.gif ${RBASEW}/analysis/nh.gif
rcp sh13.gif ${RBASEW}/analysis/sh.gif
rcp nh13.gif ${RBASEW}/analysis/nh/nh.${tag4}.gif
rcp sh13.gif ${RBASEW}/analysis/sh/sh.${tag4}.gif
rcp nh13.gif ${RBASEF}/ice/nh.gif
rcp sh13.gif ${RBASEF}/ice/sh.gif
rcp nh13.gif ${RBASEF}/ice/nh/nh.${tag4}.gif
rcp sh13.gif ${RBASEF}/ice/sh/sh.${tag4}.gif

for base in nh15 nh13 nh14 sh15 sh13 sh14
do
  mv ${base}.$tag4.xpm ${base}.xpm
    if [ -f ${base}.gif ] ; then
      rm ${base}.gif
    fi
  xpmtogif ${base}.xpm 
  rcp ${base}.gif ${RBASEW}/expt.analy/${base}.gif
done

#Copy to secondary machine:
RBASEW2=rmg3@lnx117.wwb.noaa.gov:/data/WWW/seaice
RBASEF2=rmg3@lnx117.wwb.noaa.gov:/home/ftp/pub
rcp nh13.gif ${RBASEW2}/analysis/nh.gif
rcp sh13.gif ${RBASEW2}/analysis/sh.gif
rcp nh13.gif ${RBASEW2}/analysis/nh/nh.${tag4}.gif
rcp sh13.gif ${RBASEW2}/analysis/sh/sh.${tag4}.gif
rcp nh13.gif ${RBASEF2}/ice/nh.gif
rcp sh13.gif ${RBASEF2}/ice/sh.gif
rcp nh13.gif ${RBASEF2}/ice/nh/nh.${tag4}.gif
rcp sh13.gif ${RBASEF2}/ice/sh/sh.${tag4}.gif
rcp eng.$tag4             ${RBASEF2}/cdas
rcp northpsg.${tag4}.13   ${RBASEF2}/ice/northpsg.${tag4}
rcp southpsg.${tag4}.13   ${RBASEF2}/ice/southpsg.${tag4}
rcp umasksouth.${tag4}.13 ${RBASEW2}/analysis/south
rcp umasknorth.${tag4}.13 ${RBASEW2}/analysis/north
rcp fill.$tag4            ${RBASEW2}/analysis/filled
rcp fill.$tag4            ${RBASEW2}/expt.analy/filled
for base in nh15 nh13 nh14 sh15 sh13 sh14
do
  if [ -f ${base}.gif ] ; then
    rcp ${base}.gif ${RBASEW2}/expt.analy/${base}.gif
  fi
done

