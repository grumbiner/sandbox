#!/bin/ksh
#Script to execute the Gulf Stream finder 
set -x

REMID=wd21rg
SPUSER=wx21rg

RUNDIR=/usr1/running/rungsf/
OFSBASE=${SPUSER}@prodsp:/com/cofs/prod/
FIXDIR=/usr/local/data/
EXDIR=/usr1/rmg3/gsfinder/
OUTDIR=/usr1/running/gsfout/

REMHOST=sgi100
IMGGEN=gsf/run.sh
IMGOUT=polar:/data/WWW/gsf/
DATOUT=polar:/home/ftp/pub/gsf/

PATH=$PATH:/usr/local/bin/

#---------- Should need no changes below here ----------------------
export RUNDIR OFSBASE FIXDIR EXDIR OUTDIR IMGGEN IMGOUT DATOUT REMID REMHOST 
ttag=$1
tjdy=$2
tag=${ttag:-`date +"%Y%m%d"`}
jd=${tjdy:-`date +"%j"`}
echo Date being executed: $tag $jd

if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
  if [ ! -d $RUNDIR ] ; then
    echo Could not find or construct a running directory, $RUNDIR  Failing.
    exit
  fi
fi
if [ ! -d $OUTDIR ] ; then
  mkdir -p $OUTDIR
  if [ ! -d $OUTDIR ] ; then
    echo Could not find or construct a output directory, $OUTDIR  Failing.
    exit
  fi
fi
if [ ! -d $FIXDIR ] ; then
  echo could not find the fixed files directory $FIXDIR  Failing.
  exit
fi
if [ ! -d $EXDIR ] ; then
  echo could not find the executables directory $EXDIR  Failing.
  exit
fi

cd $RUNDIR


#Get a current Navy Gulf Stream File if possible:
if [ ! -f gs${jd}nw.sub ] ; then
  scp polar:/export/s2/xbt/gs/gs${jd}nw.sub .
fi

#Parse the Navy Gulf Stream file if possible:
if [ ! -f nout.$tag -o ! -f sout.$tag ] ; then
  if [ -f gs${jd}nw.sub ] ; then
    $EXDIR/parse.pl nout sout < gs${jd}nw.sub
    cp nout nout.$tag
    cp sout sout.$tag
  fi
fi
if [ ! -f nout.$tag -o ! -f sout.$tag ] ; then
  gs=no
 else
  gs=yes
fi

# Get the OFS output ----------------------------------------
for hh in n00 f24 f48
do
  if [ ! -f cofs.t00z.$hh.agb.std ] ; then
    /usr/bin/rcp $OFSBASE/cofs.$tag/cofs.t00z.$hh.agb.std .
  fi
done
if [ ! -f cofs.t00z.n00.agb.std ] ; then
  echo Could not get cofs.t00z.n00.agb.std failing gracefully now
  exit 1
fi

# Get the fixed files ----------------------------------------
for fname in cfs.bini cfs.binj cfs.bin.full 
do
  if [ ! -f $fname ] ; then
    cp $FIXDIR/$fname .
  fi
  if [ ! -f $fname ] ; then
    echo Could not get required fixed file $FIXDIR/$fname -- failing gracefully
    exit 2
  fi
done

#Extract the needed field(s) --------------------------------

for file in n00 f24 f48
do
  wgrib cofs.t00z.$file.agb.std | grep DSLM | \
              wgrib -i cofs.t00z.$file.agb.std -nh -o dslm.$file
done
if [ ! -s dslm.n00 ] ; then
  echo Do not have a valid dslm nowcast file to work with -- failing
  exit 3
fi

# ------- At long last, run the code -----------------------
valid_date=$tag
for file in n00 f24 f48
do
  reqd_args="dslm.$file genes_out analyzed.$tag.$file"
  opt_args=" "
  if [ -f $OUTDIR/genes_in ] ; then
    opt_args="$OUTDIR/genes_in "
  fi
  if [ $gs = "yes" -a $file = "n00" ] ; then
    opt_args="$opt_args nout.$tag 65.0 77.0"
  fi
  $EXDIR/gsfinder $reqd_args $opt_args > gsout.$tag.$file

  if [ $gs = "yes" -a $file = "n00" ] ; then
    cp genes_out $OUTDIR/genes_out.$tag
    cp genes_out $OUTDIR/genes_in
  fi

  $EXDIR/form analyzed.$tag.$file reformed.$tag.$file $tag $valid_date
  valid_date=`expr $valid_date + 1`
  valid_date=`dtgfix3 $valid_date`

done

# ------- Now generate graphics and distribute data files ---
#Distribute data files:
for file in n00 f24 f48
do
  scp analyzed.$tag.$file $DATOUT
  scp reformed.$tag.$file $DATOUT
  rcp analyzed.$tag.$file ${REMID}@${REMHOST}:gsf
  rcp reformed.$tag.$file ${REMID}@${REMHOST}:gsf
  rcp cofs.t00z.$file.agb.std $REMID@$REMHOST:gsf/${tag}00${file}.agb.std 
  mv cofs.t00z.$file.agb.std ${tag}00${file}.agb.std 
done

#Construct graphics, trust IMGGEN to distribute them properly
rsh -l $REMID $REMHOST $IMGGEN $tag 
