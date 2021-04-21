for mm in '01 jan' '02 feb' '03 mar' '04 apr' '05 may'
do
  n=`echo $mm | cut -c1-2`
  name=`echo $mm | cut -c4-90`
  cat xferlog.2004${n}* > $name; ./xfst.pl < $name > $name.stats; grep count $name.stats | sort -nr > $name.counts
done
