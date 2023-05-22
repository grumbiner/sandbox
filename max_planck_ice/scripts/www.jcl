tag=`date +'%y%m%d' `

EXDIR=/data/forecasts/ice.model/crays/execs
REFDIR=/data/forecasts/ice.model/crays/icemodel/running
pole=glk

cd /export/s2/tmp
export EXDIR pole REFDIR
 
for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168
do

  $EXDIR/glktxpm conc.glk.${tag}$time $REFDIR/$pole/bathy.$pole gl$time.xpm
  xpmtogif gl$time.xpm
  mv gl$time.gif /data/WWW/develop/glk
  rm gl$time.xpm
done
if [ -s colcell1.xpm ] ; then
  rm *.xpm *.html
fi



