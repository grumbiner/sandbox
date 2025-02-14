#Operational sea ice forecast model control script
#QSUB -o /eldisk1/wd21rg/fullfore.aug
#limits should be derived from resolution. note.
#QSUB -lT  5000
#QSUB -lM     4.0Mw
#QSUB -me

RUNDIR=/tmp/wd21rg
EXDIR=/eldisk1/wd21rg/execs
FBASE=/eldisk1/wd21rg/fluxes/sflux
REFDIR=/jdsk41/wd21rg/icemodel/running
OUTDIR=/eldisk1/wd21rg/iceout2
export RUNDIR REFDIR EXDIR OUTDIR FBASE

YMBASE=9508

#-------------- No changes below this line ---------------------

if [ ! -d $OUTDIR ] ; then
  mkdir $OUTDIR
fi
if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR
rm *

FILENV=.assign
export FILENV 
dmget ${FBASE}${YMBASE}.tar 
tar xvf ${FBASE}${YMBASE}.tar

set -xe
for pole in north south 
#for pole in north 
do
  export pole
#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole} .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .
  cp ${REFDIR}/${pole}/bathy.$pole bathy

#Now loop through time within the month
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 28 29 30 31 
do
  #Collect the forecast files
  #Attach files to unit numbers
  tag=${YMBASE}$dy
  for time in 00 06 12 18
  do
    file1=flxf06.${YMBASE}${dy}$time
    file2=index.$time
    if [ -f $file2 ] ; then
      rm $file2
    fi
    assign -a $file1 -s unblocked fort.11
    assign -a $file2 -s unblocked fort.51
    ${EXDIR}/windex1 $file1 $file2
  
    i=40
    i=`expr $i + 1`
    assign -a ${file1} -s unblocked   fort.$i
    j=`expr $i + 30`
    assign -a ${file2} -s unblocked   fort.$j
  
    #Assign the output file for the translated fields
    assign -a metout.$pole           fort.20
    touch metin
    echo 1 > metin
    ja
      time ${EXDIR}/met.$pole < metin
    ja -chlst
    rm metin
  
    cp metout.${pole} metout
    chmod 644 metout

    if [ -s $OUTDIR/restart.$pole ] ; then
      $HOME41/icemodel/ic/ic.jcl 
      cp $OUTDIR/restart.$pole RESTARTo
      echo ${REFDIR}/${pole}/runparm.${pole} > runin
     else
      touch RESTARTo
      echo ${REFDIR}/${pole}/runzero.${pole} > runin
    fi
  
    ja
    time stossel.${pole} < runin
    ja -chlst
  
    rm runin
  
  #Fields for general use, to be overwritten
    cp RESTARTn $OUTDIR/restart.$pole
    cp thick    $OUTDIR/thick.$pole
    cp conc     $OUTDIR/conc.$pole
    cp vels     $OUTDIR/vels.$pole
    cp FORT.18  $OUTDIR/fort18.$pole
  
  #Create/update the date file
  #This needs to be replaced/revised. note.
    $REFDIR/datewk $tag
    mv date $OUTDIR/date.$pole
  
  #Fields for archival
    if [ ! -d $OUTDIR/$pole ] ; then
      mkdir $OUTDIR/$pole
    fi
    FDIR=$OUTDIR/$pole/${tag}$time
    if [ ! -d $FDIR ] ; then
      mkdir $FDIR
    fi
    mv RESTARTn $FDIR
    mv thick    $FDIR
    mv conc     $FDIR
    mv vels     $FDIR
    mv atm.flux $FDIR
    mv oce.flux $FDIR
    mv hml      $FDIR
    mv sml      $FDIR
    mv tml      $FDIR
    mv FORT.16  $FDIR
    mv FORT.18  $FDIR
  
  done
  #above finishes run through day for a specific region

done
#above finishes run through days
  
done
#finished all regions
qsub $HOME41/icemodel/running/restart.sep
