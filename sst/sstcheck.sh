EXDIR=lakesst/avhrr/

for dt in 3 6 10
do
  cat compout.* | $EXDIR/low.pl $dt > l${dt}.kml
  cat compout.* | $EXDIR/hinav.pl $dt > n${dt}.kml
  cat compout.* | grep -v ' 157 157 2 ' | $EXDIR/hirtg.pl $dt > r${dt}.kml
done
