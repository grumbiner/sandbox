#tag=19790101
#j=0
#while [ $tag -le 20101231 ]
#do
#  yy=`echo $tag | cut -c1-4`
#  wgrib data/$yy/icegrb.gdas.$tag | wgrib -i -nh  data/$yy/icegrb.gdas.$tag -o conc.$j 
#
#  j=`expr $j + 1`
#  tag=`expr $tag + 1`
#  tag=`dtgfix3 $tag`
#done

time ./to_time_series > tsout
