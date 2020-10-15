#Control script to run the sea ice drift forecast model
export RUNDIR=/gpfstmp/wd21rg/drift 
if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
fi
cd $RUNDIR
chmod u+w *
rm *

RHOME=/nfsuser/g01/wx21rg/sicedrft/exec
CHOME=/nfsuser/g01/wx21rg/sicedrft/fix
OHOME=/emc1/ice/driftfore/
OHOME3=rmg3@penguin:/data/forecasts/driftfore/expt

#tag=`date +'%Y%m%d'`
tag=20010611

## copy over the directory with all info/files/etc
#set -xe

cp $RHOME/sicedrft.x    .
if [ ! -f sicedrft.x ]  ; then
  echo failed to copy over model
fi

if [ ! -s $CHOME/forecast.points ] ; then
  $RHOME/skpoints > forecast.points
  cp forecast.points $CHOME
 else
  cp $CHOME/forecast.points .
fi


##make unit assignments
ln forecast.points   fort.47
cp $CHOME/quote      fort.91
echo $tag > alpha
ln alpha             fort.90

#get the ice line points
cp $CHOME/COM.IPASAIJI.NPOC.ARCTIC niceline
cp $CHOME/COM.IPASAIJI.NPOC.ANTARC siceline
ln niceline   fort.48
ln siceline   fort.49

#units for the mrf data
BASE=/com/mrf/prod/mrf.$tag/drfmr.t00z.sf
i=10
for hh in  00  12  24  36  48  60  72  84  96 108 120 132 144 156 168 180 192 \
          204 216 228 240 252 264 276 288 300 312 324 336 348 360 372 384 
do
  i=`expr $i + 1 `
  cp ${BASE}$hh fort.$i
done

#execute the model
echo 32 | ./sicedrft.x
ln fort.60 fl.out
ln fort.61 ops.out
ln fort.62 ak.out

#Distribute the output
if [ ! -d $OHOME ] ; then
  mkdir -p $OHOME
fi
cp ops.out  $OHOME/sk2.$tag
cp ak.out   $OHOME/ak.$tag
rcp ops.out wd21rg@polar:/data/WWW/seaice/drift.figs/drift.out
rcp ak.out  wd21rg@polar:/data/WWW/seaice/drift.figs/ak.out
rcp ops.out wd21rg@polar:/home/ftp/pub/ice/drift.out
rcp ak.out  wd21rg@polar:/home/ftp/pub/ice/ak.out

rm fort.??
