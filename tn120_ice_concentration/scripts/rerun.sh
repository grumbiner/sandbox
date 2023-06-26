#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/hires/batchout
#@ error=/nfsuser/g01/wx21rg/hires/errout
#@ class=1
#@ wall_clock_limit=00:10:00
#@ queue

#set -x

SYSTEM=STOMMEL

if [ $SYSTEM = "IBM" ] ; then
  #Directives for the IBM CCS:
  DTGDIR=/nfsuser/g01/marine/local/bin/
  RUNDIR=/gpfstmp/wd21rg/ssmi.high
  TMPDIR=$RUNDIR
  EXDIR=/nfsuser/g01/wx21rg/hires/execs
  FIX=/nfsuser/g01/wx21rg/hires/fix
  DATDIR=/nfsuser/g01/wx21rg/hires/fix
  OUTDIR=/emc1/ice/ice.analy.high
  SSMIDIR=/emc1/ice/ssmi85
  SSTDIR=/emc1/ice/sst.high
  OLDDIR=/emc1/ice/ice.old.high
  ADIR1=$OUTDIR
  PATH=$PATH:/nwprod/util/exec/:/nfsuser/g01/marine/local/bin:/nfsuser/g01/marine/local/bin/netpbm
  ASSESS=
else
  #Directives for Stommel:
  DTGDIR=/usr1/home/wd21rg/bin/
  RUNDIR=/usr1/dataflow/ice/team2
  OUTDIR=/usr1/dataflow/ice/team2.final
  TMPDIR=$RUNDIR
  EXDIR=/usr1/home/wd21rg/newer.ice/execs
  DATDIR=/usr1/home/wd21rg/fix
  FIX=/usr1/home/wd21rg/fix
  SSMIDIR=/usr1/dataflow/ssmi85
  SSTDIR=/usr1/dataflow/sst
  OLDDIR=/usr1/dataflow/ice.old
  UNBLKDIR=/usr1/dataflow/unblock
  ADIR1=$OUTDIR
  ASSESS=
fi

export RUNDIR TMPDIR EXDIR FIX DATDIR OUTDIR SSMIDIR SSTDIR OLDDIR ADIR1 UNBLKDIR

# ---------- Should need no changes below here ------------------
if [ ! -d $RUNDIR ] ; then
  echo about to try to make directory $RUNDIR
  mkdir -p $RUNDIR
fi
cd $RUNDIR
if [ `pwd` != $RUNDIR ] ; then
  echo Could not change directory to $RUNDIR  Failing!!!
  exit
fi
#ensure that we don't have local debris:
rm -rf *

#Script variant to catch up for a specified date.  Rebuild what needs
# to be rebuilt, and run from there.  
#If year and date tag are given use those, else rerun today 
if [ $# -ge 2 ] ; then
  yy=$1
  rest=$2
fi
yy=${yy:-`date +"%Y"`}
rest=${rest:-`date +"%m%d"`}  
tag4=${yy}$rest
tagm=`expr $tag4 - 1`
tagm=`${DTGDIR}/dtgfix3 $tagm`
echo $tagm
#dummy variable
jday=`date +"%j" `

#If there isn't an ssmi file, construct one
#don't bother with ssmi tank if we already have an n3ssmi file:
if [ ! -s $OUTDIR/n3ssmi.$tag4 ] ; then
  if [ ! -s $SSMIDIR/ssmi85.$tag4 ] ; then 
    #IBM time /nwprod/ush/dumpjb ${yy}${rest}00 12 ssmit
    echo Would have had to construct ssmi85.$tag4 on stommel, failing
    exit
  else
    #IBM cp $SSMIDIR/ssmi85.$tag4 ssmit.ibm
    ln -sf $SSMIDIR/ssmi85.$tag4 ssmit.ibm
  fi

# From stommel version, look to unblock directory:
  if [ -s $UNBLKDIR/unblock.$tag4 ] ; then
    ln $UNBLKDIR/unblock.$tag4 fort.51
  else
    #Must rerun this part (?)
    if [ -f fort.51 ] ; then
      rm fort.51
    fi
    if [ -f fort.14 ] ; then
      rm fort.14
    fi
    ln ssmit.ibm fort.14
    $ASSESS $EXDIR/seaice_ssmibufr 
  fi

fi

echo fort.51 > delta
if [ ! -f TBccant.tab ] ; then
  cp $FIX/*.tab .
fi 
  if [ ! -s $OUTDIR/n3ssmi.${tag4} -o \
       ! -s $OUTDIR/umasksouth12.$tag4 ] ; then 
    $ASSESS $EXDIR/seaice_seaissmi delta $FIX/seaice_nland127.map $FIX/seaice_sland127.map \
       n3ssmi.$tag4 s3ssmi.${tag4} \
       umasknorth12.$tag4 umasksouth12.$tag4 \
       $jday $yy 246 > v.$tag4
#Construct 25.4 km versions:  NEW:
    $ASSESS $EXDIR/seaice_north_reduce umasknorth12.$tag4 umasknorth.$tag4 \
                        $FIX/seaice_nland127.map $FIX/seaice_nland.map
    $ASSESS $EXDIR/seaice_south_reduce umasksouth12.$tag4 umasksouth.$tag4 \
                        $FIX/seaice_sland127.map $FIX/seaice_sland.map
  fi


#Make up the grib files for the polar stereographic data
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin

#Go through for each satellite and process to xpm and to grib:
newgraphics=false
#Operational version uses single exec for each resolution/grid
  if [ ! -s $OUTDIR/nh12.$tag4.xpm ] ; then
     $ASSESS $EXDIR/seaice_north12xpm umasknorth12.$tag4 $FIX/seaice_nland127.map nh12.$tag4.xpm
     convert -display lnx116:0 nh12.$tag4.xpm nh12.$tag4.gif
  fi
  if [ ! -s $OUTDIR/sh12.$tag4.xpm ] ; then
     $ASSESS $EXDIR/seaice_south12xpm umasksouth12.$tag4 $FIX/seaice_sland127.map sh12.$tag4.xpm
     convert -display lnx116:0 sh12.$tag4.xpm sh12.$tag4.gif
  fi
  if [ ! -s $OUTDIR/nh.$tag4.xpm ] ; then
     $ASSESS $EXDIR/seaice_northxpm umasknorth.$tag4 $FIX/seaice_nland.map nh.$tag4.xpm
     convert -display lnx116:0 nh.$tag4.xpm nh.$tag4.gif
  fi
  if [ ! -s $OUTDIR/sh.$tag4.xpm ] ; then
     $ASSESS $EXDIR/seaice_southxpm umasksouth.$tag4 $FIX/seaice_sland.map sh.$tag4.xpm
     convert -display lnx116:0 sh.$tag4.xpm sh.$tag4.gif
  fi

#Engribbing of PS grids:
  if [ $SYSTEM = "IBM" ] ; then
    for pole in north south north12 south12
    do
      if [ -f fort.11 ] ; then
         rm fort.11  
      fi
      if [ -f fort.51 ] ; then
         rm fort.51
      fi
      if [ ! -s $OUTDIR/${pole}psg.$tag4 ] ; then
        ln umask${pole}.$tag4 fort.11
        $ASSESS ${EXDIR}/seaice_psg${pole} < psin
        if [ -f fort.51 ] ; then
          mv fort.51 ${pole}psg.$tag4
        fi
        if [ -f fort.52 ] ; then
          mv fort.52 wmo${pole}psg.j990l
        fi
      fi
    done
  fi


## Later: move this cp into the loop and specify the * material
#Transfer files to output directory:
rm unblock.$tag4
cp *${tag4} *${tag4}*gif wmo* $OUTDIR
exit

#Now begin the global file processing:

#QC denotes whether or not it will be possible to produce the
#  SST-quality controlled sea ice grids for modellers.
#set -x
qc=false
if [ -s $SSTDIR/rtg_sst_grb_0.5.$tag4 ] ; then
  cp $SSTDIR/rtg_sst_grb_0.5.$tag4 oned
  qc=true
else
  j=$tag4
  while [ ! -s $SSTDIR/rtg_sst_grb_0.5.$j -a qc="false" ]
  do
    j=`expr $j - 1`
    j=`${DTGDIR}/dtgfix3 $j`
    if [ -s $SSTDIR/rtg_sst_grb_0.5.$j ] ; then
      cp $SSTDIR/rtg_sst_grb_0.5.$j oned
      qc=true
    fi
  done
fi

echo sst qc flag is $qc

if [ $qc = "true" ] ; then
  #Filter the ice concentration field (in lat-long space only so far)
  if [ -f sst ] ; then
    rm sst
  fi
  $EXDIR/wgrib oned | grep TMP | $EXDIR/wgrib -i oned -o sst -nh -bin
  #IBM wgrib oned | grep TMP | wgrib -i oned -o sst -nh -bin
  if [ ! -s sst ] ; then
    echo failed to get an sst field!
    exit 1
  fi

#Run the old filter as well for historic reasons -- stomp on nps, sps as
#  they are not used anyhow.
#  if [ ! -s $OLDDIR/fill.$tag4 -a $OLDDIR/age.$tag4 ] ; then
#    $EXDIR/filt.old sst umasknorth.${tag4} umasksouth.${tag4} \
#          $OLDDIR/latlon.$tag4 nps.$tag4 sps.$tag4 $FIX/global 275.3 \
#          $FIX/nland.map $FIX/sland.map
#    $EXDIR/icegrid.x $OLDDIR/fill.$tagm $OLDDIR/latlon.$tag4 $OLDDIR/age.$tagm \
#                     $OLDDIR/age.$tag4 $OLDDIR/fill.$tag4
#  fi

  if [ ! -s $OUTDIR/latlon.$tag4 ] ; then
  $ASSESS $EXDIR/seaice_filtanal sst umasknorth12.${tag4} umasksouth12.${tag4} \
        latlon.$tag4 nps.$tag4 sps.$tag4 $FIX/seaice_gland5min 275.3 \
        $FIX/seaice_nland127.map $FIX/seaice_sland127.map
  fi

  #fill in the ice concentration field with older data as required
  if [ ! -s $OUTDIR/fill5min.$tag4 ] ; then
    $ASSESS $EXDIR/seaice_icegrid $ADIR1/fill5min.$tagm latlon.$tag4 $ADIR1/age.$tagm \
                             age.$tag4 fill5min.$tag4
    retcode=$?
  fi
  $ASSESS $EXDIR/seaice_global_reduce fill5min.$tag4 fill.$tag4 $FIX/seaice_gland5min $FIX/seaice_newland
  $ASSESS $EXDIR/seaice_global5minxpm fill5min.$tag4 $FIX/seaice_gland5min global5min.$tag4.xpm
  $ASSESS $EXDIR/seaice_globalxpm fill.$tag4 $FIX/seaice_newland global.$tag4.xpm
  convert -flip -display lnx116:0 global5min.$tag4.xpm global5min.$tag4.gif
  convert -flip -display lnx116:0 global.$tag4.xpm global.$tag4.gif

  if [ $retcode -ne 0 ] ; then
    if [ $retcode -eq 1 ] ; then
      echo Failed to open an input file
    elif [ $retcode -eq 2 ] ; then
      echo Failed to open an output file
    fi
  fi


#  #Engrib the lat-long file -- low resolution:
  if [ $SYSTEM = "IBM" ] ; then
    if [ ! -s $OUTDIR/eng.$tag4 ] ; then
      echo $tag4 > ein
      ln -f fill.$tag4   fort.11
      touch fort.51
      ln -f fort.51      eng.$tag4
      $ASSESS $EXDIR/seaice_ice2grib < ein
      cp eng.$tag4    ${ADIR1}
      cp fort.52      ${ADIR1}/wmoglobice.990l
    fi
#    #Engrib the lat-long file -- high resolution:
    if [ ! -s $OUTDIR/eng5min.$tag4 ] ; then
      echo $tag4 > ein
      rm fort.11
      ln -f fill5min.$tag4   fort.11
      touch fort.51
      ln -f fort.51      eng5min.$tag4
      $ASSESS $EXDIR/seaice_ice2grib5min < ein
      cp eng5min.$tag4    ${ADIR1}
    fi
  fi

  #Remove the temporaries
  for fn in latlon.$tag4 age.$tag4 fill.$tag4 fill5min.$tag4
  do
    if [ -s $fn ] ; then
      cp $fn $ADIR1
    fi
  done

fi #end of producing qc'd files

cp global5min.$tag4.gif global.$tag4.gif $OUTDIR
#Ensure that we don't provide excess permissions on files:
chmod a-x eng* *thpsg.* umask* fill* *.gif

##########################################################
#
#
exit
#Product distribution section
RBASEW=rmg3@polar.wwb.noaa.gov:/data/WWW/seaice/expt/
RBASEF=rmg3@polar.wwb.noaa.gov:/var/ftp/pub
RBASEW1=rmg3@polar.wwb.noaa.gov:/data/WWW/seaice
RBASEF1=rmg3@polar.wwb.noaa.gov:/var/ftp/pub
RBASEW3=wd21rg@rzdm.wwb.noaa.gov:/home/people/emc/www/htdocs/mmab/seaice
RBASEF3=wd21rg@rzdm.wwb.noaa.gov:/home/people/emc/ftp/mmab/seaice
#
# Copy results over to polar, test directory
#
if [ -s eng.$tag4 ]  ; then
  scp eng.$tag4             ${RBASEF}/cdas
  scp eng5min.$tag4             ${RBASEF}/cdas
  scp north12psg.${tag4}   ${RBASEF}/ice/north12psg.${tag4}
  scp south12psg.${tag4}   ${RBASEF}/ice/south12psg.${tag4}
  scp northpsg.${tag4}   ${RBASEF}/ice/northpsg.${tag4}
  scp southpsg.${tag4}   ${RBASEF}/ice/southpsg.${tag4}
  scp umasknorth12.$tag4   ${RBASEW}/north
  scp umasksouth12.$tag4   ${RBASEW}/south
  scp fill5min.$tag4            ${RBASEW}/expt.analy/filled
  scp fill.$tag4            ${RBASEW}/expt.analy/filled
  scp $OLDDIR/fill.$tag4    ${RBASEF}/tempor/ice.old/
fi

echo "Working on xpms"
if [ -f nh12.$tag4.xpm -a $newgraphics = "true" ] ; then
  for base in nh12 sh12 
  do
    cp ${base}.$tag4.xpm ${base}.xpm
    if [ -f ${base}.gif ] ; then
      rm ${base}.gif
    fi
    scp ${base}.gif ${RBASEW}/expt.analy/${base}.gif
  done
fi

if [ $newgraphics = "true" ] ; then
  #for remote in $RBASEW1 $RBASEW2 
  for remote in $RBASEW1 
  do
    scp nh12.gif $remote/analysis/nh.gif
    scp sh12.gif $remote/analysis/sh.gif
    scp nh12.gif $remote/analysis/nh/nh.${tag4}.gif
    scp sh12.gif $remote/analysis/sh/sh.${tag4}.gif
  done
  #for remote in $RBASEF1 $RBASEF2 
  for remote in $RBASEF1 
  do
    scp nh12.gif $remote/ice/nh.gif
    scp sh12.gif $remote/ice/sh.gif
    scp nh12.gif $remote/ice/nh/nh.${tag4}.gif
    scp sh12.gif $remote/ice/sh/sh.${tag4}.gif
  done
fi

#Copy data files to secondary machine:
#Graphics now managed above by for loop

$EXDIR/makefigs.sh
