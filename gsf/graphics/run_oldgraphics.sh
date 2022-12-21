#!/bin/sh

#Script to produce graphics of the gulf stream finder output on top of cofs
#  output
#Robert Grumbine  22 January 2003
#Section for production of graphics, and codes used for that, are based
#  on standard_plot.sh by Bill O'Connor 11/2002.
#  -- Generalized for multiple input files,
#  -- some modification to loop on parameters
#Bill O'Connor 14 February 2003
#  Include proper date computations for labelling graphics
#Robert Grumbine 12 March 2003
#  Remove legacy of nowcast being 'yesterday'.

IDLPATH=/opt/rsi/idl:/opt/rsi/idl/bin:/opt/rsi/idl/lib:/opt/rsi/idl/examples
#setenv IDL_DIR /opt/rsi/idl
#setenv IDL_PATH \+$IDL_DIR/lib:\+$IDL_DIR/examples
WKDIR=/migr/people/wd21rg/gsf
today=$1

set -x
# ------------- Should need no changes below here -------------- 
PATH=$PATH:$IDLPATH
export PATH

if [ ! -d $WKDIR ] ; then
  echo Could not find the working directory, Failing!
  exit 1
fi
cd $WKDIR 

if [ $today -lt 19800101 ] ; then
  echo Date is too early, or no date given.  arg1 = $1
  exit 2
fi

echo $today 


for interval in n00 f24 f48
do
  file=${today}00$interval.agb.std
  echo $file
  # check to see if file is present
  if [ ! -f $file ] ; then
    echo "Model output $file not available for Gulf Stream Graphics"
    exit 1 
  fi 
done
#--------------------------------------------
# determine the valid date for the plot
dayafter=$today

for interval in n00 f24 f48
do
  validdate=$dayafter
  echo interval $interval
  echo validdate $validdate 

#---------------------------------------------

  #Extract the standard parameters for graphic production:
  for parm in UOGRD VOGRD WTMP SALTY
  do
    wgrib $file | grep "kpds6=160:kpds7=0" | egrep $parm \
      | wgrib -i -text $file  -o $parm.txt   
  done
  # degrib the sea level to a file wl.txt 
  wgrib $file | grep ":DSLM:" | wgrib -i -text $file -o wl.txt  

  # execute the fortran program to read these four files and reformat
  # them to one file uvst.txt 
  ./read_standard.exe 

  # execute IDL program to make plots in the postscript file plot_standard.ps
  ln -sf analyzed.$today.$interval clean.$validdate
  ln -sf analyzed.$today.$interval nout.$validdate
  #echo ".run plot_standard_rg.pro"  > idlfile
  echo ".run new.pro"  > idlfile
  echo ps                       >> idlfile
  echo $validdate               >> idlfile
  wc clean.$validdate               >> idlfile
  wc nout.$validdate                >> idlfile
  echo exit                     >> idlfile
  idl < idlfile

  mv plot_standard.ps plot$interval.ps
  convert plot$interval.ps $interval.gif
  for k in 0 1 2 3
  do
    convert -rotate -90 $interval.gif.$k $interval$k.gif
  done
  rm idlfile

#Advance the date for the next plot:
  dayafter=`/migr/data/cfspom/bin/ndate.x +24 ${dayafter}00`
  dayafter=`echo $dayafter  | cut -c1-8`
done

#Distribute graphics 
for interval in n00 f24 f48
do
  for k in 0 1 2 3
  do
    scp ${interval}${k}.gif rmg3@polar:/data/WWW/gsf/
  done
done
 
#Clean up
rm nout.* clean.*
#---------------------------------------------------------
