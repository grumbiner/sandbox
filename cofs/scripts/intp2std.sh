#!/bin/sh
#QSUB -lT 360
#QSUB -lM   8Mw
# Arg 1 is the name of the file to work from -- full path needed
# Arg 2 is the name you want for the unformatted binary output, again full
#         path
# Robert Grumbine 17 March 1998

# EXDIR  is the directory which contains various support data files
#    for COFS.  These are my own contrivance so it is necessary to
#    use exactly these files even when a similar COFS file already
#    exists
EXDIR=.
export EXDIR
TMPDIR=.
#NWPROD=/migr/data/wd21rg/nwprod

# Nothing below this point should need changing on a new system

# Now using the default temporary directory
cd $TMPDIR

#cp $EXDIR/* .
infile=$1
outfile=$2
#infile=$HOME3/a.igb
#outfile=$HOME3/beta

j=0
for tag in pds6=128:kpds7=0:     \
           pds6=128:kpds7=1:     \
           pds6=128:kpds7=258:   \
           pds6=128:kpds7=515:   \
           pds6=128:kpds7=774:   \
           pds6=128:kpds7=1549:  \
           pds6=128:kpds7=3354:  \
           pds6=128:kpds7=6706:  \
           pds6=128:kpds7=12900: \
           pds6=108:kpds7=2580:  \
           pds6=108:kpds7=5150:  \
           pds6=108:kpds7=7720:  \
           pds6=108:kpds7=10290: \
           pds6=108:kpds7=12860: \
           pds6=108:kpds7=15430: \
           pds6=108:kpds7=18000: \
           pds6=108:kpds7=20570: \
           pds6=108:kpds7=23140:
do
  wgrib $infile > index 
  echo $j $tag
  grep $tag index | grep WTMP  | wgrib -i $infile -o temp.$j -nh
  grep $tag index | grep SALTY | wgrib -i $infile -o salt.$j -nh
  grep $tag index | grep UOGRD | wgrib -i $infile -o uogrd.$j -nh
  grep $tag index | grep VOGRD | wgrib -i $infile -o vogrd.$j -nh
  j=`expr $j + 1`
done
grep pds6=1 index | grep pds7=0 | grep DSLM | wgrib -i $infile -o topog -nh

time ./toreg dummy temporary $outfile

