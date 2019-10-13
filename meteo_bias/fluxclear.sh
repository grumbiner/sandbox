tag=19990101
while [ $tag -le 20021231 ]
do
  yy=`echo $tag | cut -c1-4`
  for hh in 00 06 12 18
  do
    if [ -f metout.north.${tag}$hh -a -f metout.south.${tag}$hh ] ; then
      if [ -f ../fluxes/${yy}/flxf06.${tag}$hh ] ; then
        rm -f ../fluxes/${yy}/flxf06.${tag}$hh
       #else
        #echo could not find flxf06.${tag}$hh
      fi
    fi
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
   
