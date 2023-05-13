#!/bin/bash
#Script to create xpm files of fields of interest from ice model
#Uses ${pole}xpm to create the files, arguments are input file,
#  bathymetry file, output file, multiplier, offset.
#Robert Grumbine
#Last Modified 20 February 1997

#Environment variables:
# pole 
# EXDIR - defines the location of executables
# REFDIR - defines the location of reference files (bathymetry)
# RUNDIR - directory the model is being run in
# tag    - date tag

set -x
cd $RUNDIR

export RUNDIR EXDIR pole REFDIR

case $pole in
  north) pref=nf
   ;;
  south) pref=sf
   ;;
  glk)  pref=gl
   ;;
  *)
    echo "Pole I dont know how to handle, winging it!"
    pref=$pole
   ;;
esac
 
for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168
do

  $EXDIR/${pole}xpm conc.${pole}.${tag}$time $REFDIR/$pole/bathy.$pole ${pref}$time.xpm
  if [ -f /etc/linux  ] ; then
    xpmtogif ${pref}$time.xpm
    mv ${pref}$time.gif /data/WWW/develop/$pole
    rm $pole$time.xpm
   else
    rcp  ${pref}$time.xpm polar:/data/WWW/develop/$pole
    rm ${pref}$time.xpm
  fi

done
if [ -s colcell1.xpm ] ; then
  rm *.xpm *.html
fi
