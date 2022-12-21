#QSUB -lT 2200
#QSUB -lM   8.0Mw
#QSUB -a "Tomorrow-9:00"
#QSUB -o /jdsk40/wd21rg/execs2
#QSUB -e /jdsk40/wd21rg/execs2
#QSUB -me

#Script for handling the production and dissemination of all ssmi-derived
#  sea ice concentration files
#Robert Grumbine 1 March 1995.
#Modified for producing reanalysis files 28 November 1995.  Robert Grumbine
#Heavily modified to try to circumvent missing/overloaded file systems
#  on operational cray, and to be more robust against steps failing 
#  internal to the script.  23 January 1996.
#Modified to work from BUFR files.  Relies on dumpscript, with no
#  failover protection possible.  2 June 1997.
#Y2K, F90, cleanups 21 July 1998

qsub /jdsk40/wd21rg/execs2/y2k.jcl
################################################
#Check for required files/directories and try failover recovery
################################################
#cray 4 version
SDIR=$HOME40/execs2
CDIR=$HOME40/execs2
ADIR=/ptmpb/wd21rg/y2k
ADIR1=$ADIR
ADIR2=$ADIR
RDIR=/dm/wd21rg/fifteenth
RDIR2=/data/forecasts/ice.analy
TDIR=$TMPDIR
if [ ! -d $TMPDIR ] ; then
  mkdir -p $TDIR
fi

if [ ! -d $SDIR ] ; then
  if [ -d /dm/wd21rg/execs ] ; then
    SDIR=/dm/wd21rg/execs
  else
    cray4.bomb failed to find an execs directory for f13!!
  fi
fi

if [ ! -d $CDIR ] ; then
  if [ -d /dm/wd21rg/control ] ; then
    CDIR=/dm/wd21rg/control 
  else
    cray4.bomb failed to find a control directory for f13!!
  fi
fi

if [ ! -d $TDIR ] ; then
  if [ -d /ptmpb/wd21rg ] ; then
    TDIR=/ptmpb/wd21rg
  elif [ -d /marine/ice ] ; then
    TDIR=/marine/ice 
  elif [ -d /dm/wd21rg/tmp ] ; then
    TDIR=/dm/wd21rg/tmp
  else
    cray4.bomb failed to find room for temporary files!!
  fi
fi

if [ ! -d $ADIR ] ; then
  if [ -d $ADIR2 ] ; then
    ADIR=$ADIR2
  else
    cray4.bomb failed to find room to store output files!!
  fi
fi


export SDIR ADIR CDIR TDIR RDIR RDIR2

#Nothing below here should need changing on a new system
########################################################
#Begin execution
########################################################

cd $TDIR

set -x
tag=`date +'%y%m%d'`
tag4=`date +'%Y%m%d'`
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
jday=`date +'%j'`
refyear=`date +'%Y' `

ja

#BUFR------------------------------------------------------
#Run the dumpscript to retrieve a day's data
/nwprod/bufr/scripts/dumpjb ${tag}00 12 ssmit

#Process the bufr output into something readable
assign -a ssmit.cos -Fcos fort.14
assign -a bufrout -s unblocked fort.51
$SDIR/ssmibufr.x 
echo bufrout > delta


# Run the analysis on the files
if [ ! -f $SDIR/seaissmi.x ] ; then
  cray4.bomb Could not get the ssmi executable!!
fi
if [ ! -f $CDIR/nland.map ] ; then
  cray4.bomb Could not get the northern hemisphere land mask
fi
if [ ! -f $CDIR/sland.map ] ; then
  cray4.bomb Could not get the southern hemisphere land mask
fi

time ${SDIR}/seaissmi.x delta  ${CDIR}/nland.map ${CDIR}/sland.map \
             n3ssmi.$tag s3ssmi.$tag umasknorth.$tag umasksouth.$tag \
             b3north.$tag b3south.$tag $jday $refyear

#Exit here for testing

cp ?3ssmi.$tag ${ADIR1}
cp b3???th.$tag    ${ADIR1}
cp ?3ssmi.$tag ${ADIR2}
cp b3???th.$tag    ${ADIR2}

#Make up the grib files for the polar stereographic data
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin

for pole in north south
do
  assign -a umask$pole.$tag -s unblocked fort.11
  assign -a ${pole}psg.$tag -s unblocked fort.51
  ${SDIR}/psg${pole}.x < psin
done
cp *psg.$tag   ${ADIR1}
cp *psg.$tag   ${ADIR2}

######################################################
#Construct the graphic files for further display
######################################################
${SDIR}/rtxpm umasknorth.$tag $CDIR/nland.map nh.xpm \
              umasksouth.$tag $CDIR/sland.map sh.xpm


##############################
#Filter, re-process, etc. the data files
##############################

#QC denotes whether or not it will be possible to produce the
#  SST-quality controlled sea ice grids for modellers.
qc=false
if [ -s /scom/fnl/prod/fnl.$tag/gdas1.T00Z.sstgrb ] ; then 
  cp /scom/fnl/prod/fnl.$tag/gdas1.T00Z.sstgrb oned
  qc=true
else
  j=$tag
  while [ -d /scom/fnl/prod/fnl.$j -a qc="false" ] 
  do
    j=`expr $j - 1`
    j=`dtgfix3 $j`
    if [ -s /scom/fnl/prod/fnl.$j/gdas1.T00Z.sstgrb ] ; then 
      cp /scom/fnl/prod/fnl.$j/gdas1.T00Z.sstgrb oned
      qc=true
    fi
  done
fi

if [ $qc = "true" ] ; then
  #Filter the ice concentration field (in lat-long space only so far)
  assign -a oned -s unblocked fort.11
  $SDIR/filtanal.x oned umasknorth.$tag umasksouth.$tag latlon.$tag \
                nps.$tag sps.$tag $CDIR/halfdeg.map 275.3
  
  #fill in the ice concentration field with older data as required
  $SDIR/icegrid.x $ADIR/fill.$tagm latlon.$tag $ADIR/age.$tagm age.$tag fill.$tag
  
  #Engrib the lat-long file
  echo $tag4 > ein
  assign -a fill.$tag   -s unblocked fort.11
  assign -a eng.$tag    -s unblocked fort.51
  $SDIR/ice2grib.x < ein
  cp eng.$tag    ${ADIR1}
  cp eng.$tag    ${ADIR2}
  
  #Remove the temporaries
  if [ -s latlon.$tag ] ; then
    cp latlon.$tag $ADIR1
    mv latlon.$tag $ADIR2
  fi
  if [ -s age.$tag ] ; then
    cp age.$tag $ADIR1
    mv age.$tag $ADIR2
  fi
  if [ -s fill.$tag ] ; then
    cp fill.$tag $ADIR1
    #rcp fill.$tag polar:/data/WWW/seaice/analysis/filled
    mv fill.$tag $ADIR2
  fi
fi #end of producing qc'd files
  
ja -chlst
exit
  
##############################
#Distribute the files -- drop in a test script
##############################
#polar file disposition
rcp ?3ssmi.$tag  penguin:${RDIR2}
rcp eng.$tag     penguin:${RDIR2}
rcp *psg.$tag    penguin:${RDIR2}
rcp eng.$tag     polar:/home/ftp/pub/cdas
rcp *psg.$tag    polar:/home/ftp/pub/ice
rcp ?h.xpm       polar:/data/WWW/seaice/analysis
rcp umasknorth.$tag polar:/data/WWW/seaice/analysis/north
rcp umasksouth.$tag polar:/data/WWW/seaice/analysis/south

#alternate cray file disposition
rcp ?3ssmi.$tag  cray5:${RDIR}
rcp eng.$tag     cray5:${RDIR}
rcp eng.$tag     cray3:/ombptmp/ice/analy/fifteenth
rcp *psg.$tag    cray5:${RDIR}

