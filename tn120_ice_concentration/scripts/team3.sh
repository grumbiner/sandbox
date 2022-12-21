#!/bin/sh
#@ job_type=serial
#@ output=/u/wx21rg/newssmi.out
#@ error=/u/wx21rg/newssmi.err
#@ class=1
#@ wall_clock_limit=00:30:00
#@ account_no = RTO_T2O
#@ resources = ConsumableCpus(1) ConsumableMemory(900 MB)
#@ queue

set -x

SYSTEM=IBM

if [ $SYSTEM = "IBM" ] ; then
  #Directives for the IBM CCS:
  DCOMROOT=/dcom/
  DTGDIR=/u/wx21rg/bin/
  RUNDIR=/stmp/wd21rg/ssmi.high
  TMPDIR=$RUNDIR
  EXDIR=/u/wx21rg/save/iceconc/exec
  FIX=/nwprod/fix
  EXPTFIX=/u/wx21rg/save/fix
  OUTDIR=/u/wx21rg/noscrub/team3
  SSMIDIR=/u/wx21rg/noscrub/ssmi85/
  SSTDIR=/com/gfs/prod/sst.
  ADIR1=$OUTDIR
  PATH=$PATH:/nwprod/util/exec/:/u/wx21rg/bin/:/usrx/local/imajik/bin/
  ASSESS=time
else
  #Directives for Stommel:
  DTGDIR=/usr1/home/wd21rg/bin/
  RUNDIR=/usr1/dataflow/ice/team2
  OUTDIR=/usr1/dataflow/ice/team2.final
  TMPDIR=$RUNDIR
  EXDIR=/usr1/home/wd21rg/newer.ice/execs
  FIX=/usr1/home/wd21rg/fix
  SSMIDIR=/usr1/dataflow/ssmi85
  SSTDIR=/usr1/dataflow/sst
  UNBLKDIR=/usr1/dataflow/unblock
  ADIR1=$OUTDIR
  ASSESS=
fi

export DCOMROOT RUNDIR TMPDIR EXDIR FIX OUTDIR SSMIDIR SSTDIR ADIR1 UNBLKDIR

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
  if [ ! -s $SSMIDIR/$yy/ssmi85.$tag4 ] ; then 
    #IBM time /nwprod/ush/dumpjb ${yy}${rest}00 12 ssmit
    $ASSESS /nwprod/ush/dumpjb ${yy}${rest}00 12 ssmit
    #stommel echo Would have had to construct ssmi85.$tag4 on stommel, failing
    #stommel exit
  else
    #IBM cp $SSMIDIR/ssmi85.$tag4 ssmit.ibm
    ln -sf $SSMIDIR/$yy/ssmi85.$tag4 ssmit.ibm
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
if [ ! -f seaice_TBccant.tab ] ; then
  cp $FIX/*.tab .
fi 

#248 -> use F-15.  Code rewritten March 2009 to run only for a given satellite
#  246 is F-13, which is now (22 Jan 2010) defunct
if [ ! -s $OUTDIR/n3ssmi.${tag4} -o \
     ! -s $OUTDIR/umasksouth12.$tag4 ] ; then 
  $ASSESS $EXDIR/new.seaice_seaissmi delta $EXPTFIX/seaice_nland127.map $EXPTFIX/seaice_sland127.map \
     n3ssmi.$tag4 s3ssmi.${tag4} \
     umasknorth12.$tag4 umasksouth12.$tag4 \
     $jday $yy 248 > v.$tag4
#Construct 25.4 km versions:  NEW:
  $ASSESS $EXDIR/seaice_north_reduce umasknorth12.$tag4 umasknorth.$tag4 \
                      $EXPTFIX/seaice_nland127.map $EXPTFIX/seaice_nland.map
  $ASSESS $EXDIR/seaice_south_reduce umasksouth12.$tag4 umasksouth.$tag4 \
                      $EXPTFIX/seaice_sland127.map $EXPTFIX/seaice_sland.map
fi


#Go through for each satellite and process to xpm and to grib:
newgraphics=false
#Operational version uses single exec for each resolution/grid
  if [ -s $OUTDIR/umasknorth12.$tag4 ] ; then
    cp $OUTDIR/umasknorth12.$tag4 .
    cp $OUTDIR/umasksouth12.$tag4 .
    cp $OUTDIR/umasknorth.$tag4 .
    cp $OUTDIR/umasksouth.$tag4 .
  fi
  if [ ! -s $OUTDIR/nh12.$tag4.gif ] ; then
     $ASSESS $EXDIR/seaice_north12xpm umasknorth12.$tag4 $EXPTFIX/seaice_nland127.map nh12.$tag4.xpm
     convert nh12.$tag4.xpm nh12.$tag4.gif
  fi
  if [ ! -s $OUTDIR/sh12.$tag4.gif ] ; then
     $ASSESS $EXDIR/seaice_south12xpm umasksouth12.$tag4 $EXPTFIX/seaice_sland127.map sh12.$tag4.xpm
     convert sh12.$tag4.xpm sh12.$tag4.gif
  fi
  if [ ! -s $OUTDIR/nh.$tag4.gif ] ; then
     $ASSESS $EXDIR/seaice_northxpm umasknorth.$tag4 $EXPTFIX/seaice_nland.map nh.$tag4.xpm
     convert nh.$tag4.xpm nh.$tag4.gif
  fi
  if [ ! -s $OUTDIR/sh.$tag4.gif ] ; then
     $ASSESS $EXDIR/seaice_southxpm umasksouth.$tag4 $EXPTFIX/seaice_sland.map sh.$tag4.xpm
     convert sh.$tag4.xpm sh.$tag4.gif
  fi

#Make up the grib files for the polar stereographic data
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin

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


#Stop here if we don't have sst info to make global grids from
#exit
#Now begin the global file processing:

#QC denotes whether or not it will be possible to produce the
#  SST-quality controlled sea ice grids for modellers.
#set -x
qc=false
trials=0
if [ -s ${SSTDIR}$tag4/rtgssthr_grb_0.083 ] ; then
  cp ${SSTDIR}$tag4/rtgssthr_grb_0.083 oned
  qc=true
else
  j=$tag4
  while [ ! -s ${SSTDIR}$j/rtgssthr_grb_0.083 -a $qc="false"  ]
  do
    j=`expr $j - 1`
    j=`${DTGDIR}/dtgfix3 $j`
    if [ -s ${SSTDIR}$j/rtgssthr_grb_0.083 ] ; then
      cp ${SSTDIR}$j/rtgssthr_grb_0.083 oned
      qc=true
    fi
  done
  trials=`expr $trials + 1`
fi

echo sst qc flag is $qc

if [ $qc = "true" ] ; then
  #Filter the ice concentration field (in lat-long space only so far)
  if [ -f sst ] ; then
    rm sst
  fi
  if [ $SYSTEM = "IBM" ] ; then
    wgrib oned | grep TMP | wgrib -i oned -o sst -nh -bin
  else
    $EXDIR/wgrib oned | grep TMP | $EXDIR/wgrib -i oned -o sst -nh -bin
  fi
  if [ ! -s sst ] ; then
    echo failed to get an sst field!
    exit 1
  fi

  if [ ! -s $OUTDIR/latlon.$tag4 ] ; then
  $ASSESS $EXDIR/seaice_filtanal sst umasknorth12.${tag4} umasksouth12.${tag4} \
        latlon.$tag4 nps.$tag4 sps.$tag4 $FIX/seaice_gland5min 275.3 \
        $EXPTFIX/seaice_nland127.map $EXPTFIX/seaice_sland127.map
  fi

  #fill in the ice concentration field with older data as required
  if [ ! -s $OUTDIR/fill5min.$tag4 ] ; then
    $ASSESS $EXDIR/seaice_icegrid $ADIR1/fill5min.$tagm latlon.$tag4 $ADIR1/age.$tagm \
                             age.$tag4 fill5min.$tag4
    retcode=$?
  fi
  $ASSESS $EXDIR/seaice_global_reduce fill5min.$tag4 fill.$tag4 $FIX/seaice_gland5min $EXPTFIX/seaice_newland
  $ASSESS $EXDIR/seaice_global5minxpm fill5min.$tag4 $FIX/seaice_gland5min global5min.$tag4.xpm
  $ASSESS $EXDIR/seaice_globalxpm fill.$tag4 $EXPTFIX/seaice_newland global.$tag4.xpm
  convert -flip global5min.$tag4.xpm global5min.$tag4.gif
  convert -flip global.$tag4.xpm global.$tag4.gif

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
  #ls -lst eng*

#    #Engrib the lat-long file -- high resolution:
    if [ ! -s $OUTDIR/eng5min.$tag4 ] ; then
      echo $tag4 > ein
      rm fort.11 fort.51
      ln -f fill5min.$tag4   fort.11
      touch fort.51
      ln -f fort.51      eng5min.$tag4
      $ASSESS $EXDIR/seaice_ice2grib5min < ein
      cp eng5min.$tag4    ${ADIR1}
    fi
  fi

  #ls -lst eng*

  #Remove the temporaries
  for fn in latlon.$tag4 age.$tag4 fill.$tag4 fill5min.$tag4
  do
    if [ -s $fn ] ; then
      cp $fn $ADIR1
    fi
  done

fi #end of producing qc'd files

  #ls -lst eng*

#Ensure that we don't provide excess permissions on files:
chmod a-x eng* *psg*.* umask* fill* *.gif
cp global5min.$tag4.gif global.$tag4.gif $OUTDIR


#29 April 2010: Don't distribute files from test mode
exit
##########################################################
#
#
#Product distribution section
#  24 Dec 2009 -- replace with gethighres.sh -- works from ops files
/u/wx21rg/save/iceconc/distrib.ops/gethighres.sh

#Make additional figures:
#
$EXDIR/makefigs.sh
