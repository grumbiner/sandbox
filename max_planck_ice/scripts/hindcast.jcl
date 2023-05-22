#Operational sea ice forecast model control script
#QSUB -o /eldisk1/wd21rg/hindcast
#limits should be derived from resolution. note.
#QSUB -lT  1400 
#QSUB -lM     4.0Mw
#QSUB -me

RUNDIR=/tmp/wd21rg/icemodel
EXDIR=/eldisk1/wd21rg/execs
REFDIR=/jdsk41/wd21rg/icemodel/running
OUTDIR=/eldisk1/wd21rg/mpifnoc

export RUNDIR REFDIR EXDIR OUTDIR 

#-------------- No changes below this line ---------------------


if [ ! -d $OUTDIR ] ; then
  mkdir $OUTDIR
fi
if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

FILENV=.assign
export FILENV 

tag=940801
export tag
mtag=`echo $tag | cut -c1-4`
tar xvf /eldisk1/wd21rg/zflux/met${mtag}.tar
CHOME=$HOME41/icemodel/running
TDIR=$RUNDIR
export CHOME TDIR


ja
while [ $tag -le 940831 ] 
do
for pole in north south 
do
  export pole
#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole} .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .
  cp ${REFDIR}/${pole}/bathy.$pole bathy

  #Collect the forecast files
  #Attach files to unit numbers

  for time in 00 06 12 18
  do
    if [ $time  -eq 00 ] ; then
      /jdsk40/wd21rg/mpifnoc/ic.jcl .TRUE. .FALSE. >> $OUTDIR/$pole.icout
    fi
    cp met${tag}$time.${pole} metout
    chmod 644 metout

    if [ -s $OUTDIR/restart.$pole ] ; then
      cp $OUTDIR/restart.$pole RESTARTo
      echo ${REFDIR}/${pole}/runparm.${pole} > runin
     else
      touch RESTARTo
      echo ${REFDIR}/${pole}/runzero.${pole} > runin
    fi
  
    stossel.${pole} < runin
  
    rm runin
  
  #Fields for archival
    if [ ! -d $OUTDIR/mpi.$pole ] ; then
      mkdir $OUTDIR/mpi.$pole
    fi
    FDIR=$OUTDIR/mpi.$pole/${tag}$time
    if [ ! -d $FDIR ] ; then
      mkdir $FDIR
    fi
    tar cvf $FDIR/$pole.tar RESTARTn thick conc vels atm.flux oce.flux hml sml tml FORT.16 FORT.18
    cp RESTARTn $OUTDIR/restart.$pole
    mv FORT.18  $FDIR
    rm RESTARTn thick conc  vels  atm.flux oce.flux hml sml tml FORT.16  
  done
  #above finishes run through day for a specific region

done #finished all regions
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done #finished through time period
    ja -chlst
