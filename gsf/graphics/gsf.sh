#!/bin/ksh
#Script to execute the Gulf Stream finder 
#Robert Grumbine 18 January 2003
# variant for routine CCS Execution 3 June 2003

set -x
SPUSER=wx21rg

# Declarations for LINUX
RUNDIR=/usr1/running/rungsf
OUTDIR=/usr1/running/gsfout/
EXDIR=/usr1/rmg3/gulf/execs/
OFSBASE=${SPUSER}@mist:/com/cofs/prod/
FIXDIR=/usr/local/data/
PATH=$PATH:/usr/local/bin/
GSDIR=rmg3@polar:/export/xbt/gs/
SYS=LINUX

#Declarations for FROST/SNOW/BLUE/MIST/DEW
OFSBASE=/com/cofs/prod/
FIXDIR=/nwprod/fix/
RUNDIR=/u/wx21rg/scrub/rofs_gsf/rungsf
OUTDIR=/u/wx21rg/noscrub/rofs_gsf/gsfout
EXDIR=/u/wx21rg/save/rofs_gsf/execs
GSDIR=/dcom/us007003/
PATH=$PATH:/nwprod/util/exec:/u/wx21rg/bin/
SYS=IBM


DATOUT=rmg3@polar:/var/ftp/pub/gsf

#RUNDIR is the directory to run in
#OUTDIR is the directory which will receive the output
#OFSBASE is the base name of the directory for Ocean Forecast System output
#FIXDIR is the name of the directory with fixed files -- cfs.bini, cfs.binj, 
#   cfs.bin.full
#EXDIR is the directory with the executables -- gsf_parse.pl, gsfinder, gsf_form
#GSDIR is the base directory that has the Gulf Stream Analysis (if any)

# Common Declarations:
#---------- Should need no changes below here ----------------------
export RUNDIR OFSBASE FIXDIR EXDIR OUTDIR GSDIR 

#set -x

ttag=$1
tjdy=$2
tag=${ttag:-`date +"%Y%m%d"`}
jd=${tjdy:-`date +"%j"`}
jdm=`expr $jd - 1`
if [ $jdm -eq 0 ] ; then
  jdm=365
fi
echo Date being executed: $tag $jd
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`

#For running ndays before the present:
#k=0
#ndays=1
#while [ $k -lt $ndays ] 
#do 
#  tag=$tagm
#  tagm=`expr $tag - 1`
#  tagm=`dtgfix3 $tagm`
#  jd=$jdm
#  jdm=`expr $jdm - 1`
#  k=`expr $k + 1`
#done
##end messing around to make it older

if [ $jdm -lt 100 -a $jdm -ge 10 -a `echo $jdm | cut -c1` != 0 ] ; then
  jdm=0$jdm
else 
  if [ $jdm -lt 10 -a `echo $jdm | cut -c1-2` != 00 ] ; then
    jdm=00$jdm
  fi 
fi

if [ $jd -lt 100 -a $jd -ge 10 -a `echo $jd | cut -c1` != 0 ] ; then
  jd=0$jd
else
  if [ $jd -lt 10 -a `echo $jd | cut -c1-2` != 00 ] ; then
    jd=00$jd
  fi 
fi 

#Increment may be needed because of somewhat peculiar timing issues on date 
#  of arrival of the North Wall file (gsJJJnw.sub) and the valid date.

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
fi
if [ ! -d $EXDIR ] ; then
  echo could not find the executables directory $EXDIR  Failing.
fi

#-------------- Done checking for directories and setting up some of them -----
cd $RUNDIR
if [ `pwd` != $RUNDIR ] ; then
  echo Could not change to running directory
  exit 5
fi


#Get a current Gulf Stream File if possible:
if [ $SYS = IBM ] ; then
  if [ ! -f gs${jd}nw.sub ] ; then
    cp $GSDIR/$tag/wtxtbul/gs${jd}nw.sub .
  fi
  if [ ! -f gs${jd}nw.sub ] ; then
    cp $GSDIR/$tagm/wtxtbul/gs${jdm}nw.sub gs${jd}nw.sub
  fi
  if [ ! -f gs${jd}nw.sub ] ; then
    cp $GSDIR/$tag/wtxtbul/gs${jdm}nw.sub gs${jd}nw.sub
  fi
elif [ $SYS = LINUX ] ; then
  if [ ! -f gs${jd}nw.sub ] ; then 
    scp rmg3@polar:/export/xbt/gs/gs${jd}nw.sub .
  fi
else
  echo Unknown system $SYS , exiting
  exit 6
fi

#Parse the Gulf Stream file if possible:
if [ ! -f nout.$tag -o ! -f sout.$tag ] ; then
  if [ -f gs${jd}nw.sub ] ; then
    $EXDIR/gsf_parse.pl nout sout < gs${jd}nw.sub
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
  #On CCS pull over files every time
  if [ ! -f cofs.t00z.$hh.agb.std -o $SYS = IBM ] ; then
    scp $OFSBASE/cofs.$tag/cofs.t00z.$hh.agb.std .
  fi
done
if [ ! -f cofs.t00z.n00.agb.std ] ; then
  echo Could not get cofs.t00z.n00.agb.std failing gracefully now
  exit 1
fi

# Get the fixed files ----------------------------------------
if [ $SYS = IBM ] ; then
  toget='cofs_intp_cfs.bini cofs_intp_cfs.binj cofs_intp_cfs.bin.full'
elif [ $SYS = LINUX ]  ; then
  toget='cfs.bini cfs.binj cfs.bin.full'
fi

for fname in $toget
do
  if [ $SYS = IBM ] ; then
    fn2=`echo $fname | cut -f3 -d\_`
    if [ ! -f $fn2 ] ; then
      cp $FIXDIR/$fname $fn2
    fi
  else
    if [ ! -f $fname ] ; then
      cp $FIXDIR/$fname .
    fi
  fi
  if [ ! -f $fn2 ] ; then
    echo Could not get required fixed file $FIXDIR/$fn2 -- failing gracefully
    exit 2
  fi
done

#Extract the needed model output field(s) --------------------------------

for file in n00 f24 f48
do
  wgrib cofs.t00z.$file.agb.std | grep DSLM | \
              wgrib -i cofs.t00z.$file.agb.std -nh -o dslm.$file
  wgrib -v cofs.t00z.$file.agb.std | grep WTMP | grep "80.160.0" |
              wgrib -i cofs.t00z.$file.agb.std -nh -o wtmp.$file

done
if [ ! -s dslm.n00 ] ; then
  echo Do not have a valid dslm nowcast file to work with -- failing
  exit 3
fi
if [ ! -s wtmp.n00 ] ; then
  echo Do not have a valid wtmp nowcast file to work with -- failing
  exit 4
fi

# ------- At long last, run the genetic code -----------------------
valid_date=$tag
for file in n00 f24 f48
do
  reqd_args="dslm.$file wtmp.$file genes_out analyzed.$tag.$file"
  opt_args=" "
  if [ -f $OUTDIR/genes_in ] ; then
    opt_args="$OUTDIR/genes_in "
  fi
  if [ $gs = "yes" -a $file = "n00" ] ; then
    opt_args="$opt_args nout.$tag 65.0 77.0"
  fi
  time $EXDIR/gsfinder $reqd_args $opt_args > gsout.$tag.$file

  if [ $gs = "yes" -a $file = "n00" ] ; then
    cp genes_out $OUTDIR/genes_out.$tag
    cp genes_out $OUTDIR/genes_in
  fi

  $EXDIR/gsf_form analyzed.$tag.$file reformed.$tag.$file $tag $valid_date
  valid_date=`expr $valid_date + 1`
  valid_date=`dtgfix3 $valid_date`

done

# ------- Put data files into archive:
for file in n00 f24 f48
do
  mv analyzed.$tag.$file $OUTDIR/analyzed.$tag.$file
  mv reformed.$tag.$file $OUTDIR/reformed.$tag.$file
done

# ------- Now generate graphics and distribute data files ---
$EXDIR/../graphics/rofs_gsf_graphics.sh

exit

#Distribute data files:
#obsoleted by Bill O'Connor's process 21 January 2004
for file in n00 f24 f48
do
  scp analyzed.$tag.$file $DATOUT/analyzed.$tag.$file
  scp reformed.$tag.$file $DATOUT/reformed.$tag.$file
  mv cofs.t00z.$file.agb.std ${tag}00${file}.agb.std 
done
