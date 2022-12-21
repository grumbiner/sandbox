#!/bin/ksh
#Script to execute the Gulf Stream finder 

RUNDIR=/usr1/rmg3/disk1/rungsf/
OFSBASE=wx21rg@devsp:/nfsuser/g01/cfspom/output/cfs3.6_out
FIXDIR=/usr/local/data/
EXDIR=/usr1/rmg3/gsfinder/
OUTDIR=/usr1/rmg3/disk1/gsfout/

REMID=wd21rg
REMHOST=sgi100
IMGGEN=gsf/run.sh
IMGOUT=polar:/data/WWW/gsf/
DATOUT=polar:/var/ftp/pub/gsf/

#---------- Should need no changes below here ----------------------
export RUNDIR OFSBASE FIXDIR EXDIR OUTDIR IMGGEN IMGOUT DATOUT REMID REMHOST 

tag=`date +"%Y%m%d"`
jd=`date +"%j"`
#For manual overrides:
tag=$1
jd=$2

tagm=`expr $tag - 1` 
tagm=`dtgfix3 $tagm`
stag=`echo $tag | cut -c1-6`
stagm=`echo $tagm | cut -c1-6`

jdm=`expr $jd - 1`
if [ $jdm -eq 0 ] ; then
  jdm = 365
fi

#Need this because the jdm is computed, rather than from date
if [ $jdm -lt 100 -a $jdm -ge 10 ] ; then
  jdm=0$jdm
elif [ $jdm -lt 10 ] ; then
  jdm=00$jdm
fi

if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
  if [ ! -d $RUNDIR ] ; then
    echo Could not find or construct a running directory, $RUNDIR  Failing.
    exit
  fi
fi
if [ ! -d $FIXDIR ] ; then
  echo could not find the fixed files directory $FIXDIR  Failing.
fi
if [ ! -d $EXDIR ] ; then
  echo could not find the executables directory $EXDIR  Failing.
fi

cd $RUNDIR


#Get a current Gulf Stream File if possible:
if [ ! -f gs${jdm}nw.sub  -a ! -f gs${jd}nw.sub ] ; then
  rcp polar:/export/xbt/gs/gs${jdm}nw.sub .
  rcp polar:/export/xbt/gs/gs${jd}nw.sub .
fi


#Parse the Gulf Stream file if possible:
if [ ! -f nout.$tag -o ! -f sout.$tag ] ; then
  if [ -f gs${jd}nw.sub ] ; then
    $EXDIR/parse.pl nout sout < gs${jd}nw.sub
    cp nout nout.$tag
    cp sout sout.$tag
  elif [ -f gs${jdm}nw.sub ] ; then
    $EXDIR/parse.pl nout sout < gs${jdm}nw.sub
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
# Note that the nowcast is for yesterday's date (tagm)
if [ ! -f ${tagm}00n00.agb.std ] ; then 
  rcp $OFSBASE/nowcast/$stag/grib/standard/${tagm}00n00.agb.std .
fi
if [ ! -f ${tagm}00n00.agb.std ] ; then
  echo Could not get ${tagm}00n00.agb.std  -- failing gracefully now
  exit 1
fi
for hh in 24 48
do
  if [ ! -f ${tag}00f${hh}.agb.std ] ; then
    rcp $OFSBASE/forecast/$stag/grib/standard/${tag}00f${hh}.agb.std .
  fi
done

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
wgrib ${tagm}00n00.agb.std | grep DSLM | \
            wgrib -i ${tagm}00n00.agb.std -nh -o dslm.n00

for file in f24 f48
do
  wgrib ${tag}00${file}.agb.std | grep DSLM | \
              wgrib -i ${tag}00${file}.agb.std -nh -o dslm.$file
done
if [ ! -s dslm.n00 ] ; then
  echo Do not have a valid dslm nowcast file to work with -- failing
  exit 3
fi

# ------- At long last, run the code -----------------------
valid_date=$tag
for file in n00 f24 f48
do
  reqd_args="dslm.$file genes_out analyzed.$file"
  #echo $reqd_args
  opt_args=" "
  if [ -f $OUTDIR/genes_in ] ; then
    #echo found genes_in
    opt_args="$OUTDIR/genes_in "
  fi
  if [ $gs = "yes" -a $file = "n00" ] ; then
    opt_args="$opt_args nout.$tag 65.0 77.0"
  fi
  #echo $opt_args
  $EXDIR/gsfinder $reqd_args $opt_args > gsout.$tag.$file

  if [ $gs = "yes" -a $file = "n00" ] ; then
    cp genes_out $OUTDIR/genes_out.$tag
    cp genes_out $OUTDIR/genes_in
  fi

  $EXDIR/form analyzed.$file reformed.$file $tag $valid_date
  valid_date=`expr $valid_date + 1`
  valid_date=`dtgfix3 $valid_date`

done

# ------- Now generate graphics and distribute data files ---
#Distribute data files:
for file in n00 f24 f48
do
  rcp analyzed.$file $DATOUT/expt.analyzed.$file
#  rcp analyzed.$file ${REMID}@${REMHOST}:gsf
  rcp reformed.$file $DATOUT/expt.reformed.$file
#  rcp reformed.$file ${REMID}@${REMHOST}:gsf
#  if [ $file = "n00" ] ; then
#    rcp ${tagm}00$file.agb.std $REMID@$REMHOST:gsf
#   else
#     rcp ${tag}00$file.agb.std $REMID@$REMHOST:gsf
  fi
done

#Construct graphics, trust IMGGEN to distribute them properly
#echo  rsh -l $REMID $REMHOST $IMGGEN $tag $tagm
#rsh -l $REMID $REMHOST ls -lastr gsf
#rsh -l $REMID $REMHOST $IMGGEN $tag $tagm
