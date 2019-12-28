#QSUB -o  /wd2/wd21/wd21rg/ssmi.getout
#QSUB -lT 300
#QSUB -lM   8.0Mw
#QSUB -me

#Script for handling the production and dissemination of all ssmi-derived
#  sea ice concentration files
#Bob Grumbine 1 March 1995.
#Modified for producing reanalysis files 28 November 1995.  Bob Grumbine
#Heavily modified to try to circumvent missing/overloaded file systems
#  on operational cray, and to be more robust against steps failing 
#  internal to the script.  23 January 1996.


################################################
#Check for required files/directories and try failover recovery
################################################
SDIR=/wd2/wd21/wd21rg/execs
CDIR=/wd2/wd21/wd21rg/control
ADIR=/ombptmp/ice/analy/fifteenth
ADIR1=/ombptmp/ice/analy/fifteenth
ADIR2=/dm/wd21rg/ssmi/fifteenth
RDIR=/marine/ice/ice.analy
RDIR2=/data/forecasts/ice.analy
#TDIR=/tmp/wd21rg
TDIR=/ombptmp/ice/analy/fifteenth

if [ ! -d $SDIR ] ; then
  if [ -d /ombptmp/ice/execs ] ; then
    SDIR=/ombptmp/ice/execs
  else
    f13.bomb failed to find an execs directory for f13!!
  fi
fi

if [ ! -d $CDIR ] ; then
  if [ -d /ombptmp/ice/control ] ; then
    CDIR=/ombptmp/ice/control 
  else
    f13.bomb failed to find a control directory for f13!!
  fi
fi

if [ ! -d $TDIR ] ; then
  if [ -d /ptmp1/wd21rg ] ; then
    TDIR=/ptmp1/wd21rg
  elif [ -d /ptmp2/wd21rg ] ; then
    TDIR=/ptmp2/wd21rg
  elif [ -d /ombptmp/ice/analy ] ; then
    TDIR=/ombptmp/ice/analy 
  elif [ -d /dm/wd21rg ] ; then
    TDIR=/dm/wd21rg
  else
    f13.bomb failed to find room for temporary files!!
  fi
fi

if [ ! -d $ADIR ] ; then
  if [ -d /dm/wd21rg/ssmi/fifteenth ] ; then
    ADIR=/dm/wd21rg/ssmi/fifteenth
  else
    f13.bomb failed to find room to store output files!!
  fi
fi


export SDIR ADIR CDIR TDIR RDIR RDIR2

#Nothing below here should need changing on a new system
########################################################
#Begin execution
########################################################


cd $TDIR

set -x
#tag=`date +'%y%m%d'`
tag=970308
for tag in 970308 970309 970310
#for tag in 970308 
do

tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
jday=`date +'%j'`
refyear=`date +'%Y' `

ja

sstfil=/scom/fnl/prod/fnl.$tag/gdas1.T00Z.sstgrb
if [ -f $sstfil ] ; then
  cp $sstfil oned
 else
  rcp cray4:$sstfil oned
fi
qc=
if [ ! -s oned ] ; then
  echo Failed to retrieve an sst file, cannot produce qcd ice file
  qc=false
 else
  wgrib oned | wgrib -i oned -o sstout
  #Filter the ice concentration field (in lat-long space only so far)
  $SDIR/filtbg sstout b3north.$tag b3south.$tag latlon.$tag \
                nps.$tag sps.$tag 275.3
  qc=true
fi

if [ $qc = true ] ; then 
  #fill in the ice concentration field with older data as required
  $SDIR/tfill $ADIR/fill.$tagm latlon.$tag $ADIR/age.$tagm age.$tag fill.$tag
  #Engrib the lat-long file
  echo $tag > ein
  assign -a fill.$tag   -s unblocked fort.10
  assign -a eng.$tag    -s unblocked fort.12
  $SDIR/mkllglob < ein
  cp eng.$tag    ${ADIR1}
  cp eng.$tag    ${ADIR2}
 else
  echo need a means of handling the no qc file case!
fi

#Remove the temporaries
if [ -s latlon.$tag ] ; then
  cp latlon.$tag $ADIR1
  cp latlon.$tag $ADIR2
#  rm $ADIR1/latlon.$tagm
#  rm $ADIR2/latlon.$tagm
fi
if [ -s age.$tag ] ; then
  cp age.$tag $ADIR1
  cp age.$tag $ADIR2
#  rm $ADIR1/age.$tagm
#  rm $ADIR2/age.$tagm
fi
if [ -s fill.$tag ] ; then
  cp fill.$tag $ADIR1
  rcp fill.$tag polar:/data/WWW/seaice/analysis/filled
  cp fill.$tag $ADIR2
#  rm $ADIR1/fill.$tagm
#  rm $ADIR2/fill.$tagm
fi


##############################
#Distribute the files
##############################
#J916 file disposition
if [ $qc = true ] ; then
  rcp eng.$tag     cray4:${RDIR}
fi

#polar file disposition

if [ $qc = true ] ; then
  rcp eng.$tag     polar:${RDIR2}
  rcp eng.$tag     polar:/home/ftp/pub/cdas
fi

ja -chlst

done
