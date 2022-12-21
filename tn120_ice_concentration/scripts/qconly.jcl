#QSUB -o  /jdsk40/wd21rg/ssmi.getout
#QSUB -lT 180
#QSUB -lM   4.0Mw
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


################################################
#Check for required files/directories and try failover recovery
################################################
#cray 3 version
#SDIR=/wd2/wd21/wd21rg/execs
#CDIR=/wd2/wd21/wd21rg/control
#ADIR=/ombptmp/ice/analy/fifteenth
#ADIR1=/ombptmp/ice/analy/fifteenth
#ADIR2=/dm/wd21rg/ssmi/fifteenth
#RDIR=/marine/ice/ice.analy
#cray 4 version
SDIR=$HOME40/execs
CDIR=$HOME40/ssmi.oper/control
ADIR=$HOME2/fifteenth
ADIR1=$HOME2/fifteenth
ADIR2=/marine/ice/ice.analy
RDIR=/ombptmp/ice/analy/fifteenth

RDIR2=/data/forecasts/ice.analy
TDIR=/tmp/wd21rg

if [ ! -d $SDIR ] ; then
  if [ -d $HOME2/execs ] ; then
    SDIR=$HOME2/execs
  else
    cray4.bomb failed to find an execs directory for f13!!
  fi
fi

if [ ! -d $CDIR ] ; then
  if [ -d $HOME2/control ] ; then
    CDIR=$HOME2/control 
  else
    cray4.bomb failed to find a control directory for f13!!
  fi
fi

if [ ! -d $TDIR ] ; then
  if [ -d /ptmpb/wd21rg ] ; then
    TDIR=/ptmpb/wd21rg
  elif [ -d /marine/ice ] ; then
    TDIR=/marine/ice 
  elif [ -d $HOME2/tmp ] ; then
    TDIR=$HOME2/tmp
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
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
jday=`date +'%j'`
refyear=`date +'%Y' `

ja

##############################
#Filter, re-process, etc. the data files
##############################
#Get the current operational SST field
#fetch oned -mVS -fTR -t'dsn=nmc.prod.sstoi.analysis,disp=shr'
#rcp cray4:/scom/fnl/prod/sst.$tagm/sstoi_anal oned
#assign -a oned -F null -N ibm fort.11
#assign -a sstout -s unblocked fort.12
#$SDIR/oisst 

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
  wgrib oned | wgrib -i oned -o sstout
  #Filter the ice concentration field (in lat-long space only so far)
  $SDIR/filtbg sstout umasknorth.$tag umasksouth.$tag latlon.$tag \
                nps.$tag sps.$tag $CDIR/halfdeg.map 275.3
  
  #fill in the ice concentration field with older data as required
  $SDIR/tfill $ADIR/fill.$tagm latlon.$tag $ADIR/age.$tagm age.$tag fill.$tag
  
  #Engrib the lat-long file
  echo $tag > ein
  assign -a fill.$tag   -s unblocked fort.10
  assign -a eng.$tag    -s unblocked fort.12
  $SDIR/mkllglob < ein
  cp eng.$tag    ${ADIR1}
  cp eng.$tag    ${ADIR2}
  
  #Remove the temporaries
  if [ -s latlon.$tag ] ; then
    cp latlon.$tag $ADIR1
    mv latlon.$tag $ADIR2
    rm $ADIR1/latlon.$tagm
    rm $ADIR2/latlon.$tagm
  fi
  if [ -s age.$tag ] ; then
    cp age.$tag $ADIR1
    mv age.$tag $ADIR2
    rm $ADIR1/age.$tagm
    rm $ADIR2/age.$tagm
  fi
  if [ -s fill.$tag ] ; then
    cp fill.$tag $ADIR1
    rcp fill.$tag polar:/data/WWW/seaice/analysis/filled
    mv fill.$tag $ADIR2
    rm $ADIR1/fill.$tagm
    rm $ADIR2/fill.$tagm
  fi
  else
    echo qc = $qc
fi #end of producing qc'd files
  
  
##############################
#Distribute the files
##############################
#C-90 file disposition
rcp eng.$tag     cray3:${RDIR}

#polar file disposition
rcp eng.$tag     polar:${RDIR2}
rcp eng.$tag     polar:/home/ftp/pub/cdas

ja -chlst