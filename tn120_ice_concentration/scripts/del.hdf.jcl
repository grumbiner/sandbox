tag=930901

while [ $tag -le 931130 ]
do

  base=`echo $tag | cut -c1-4`
  yy=`echo $tag  | cut -c1-2`
  hdftor8 ../19${yy}/${tag}.TNN 
  ./deln ${base}.n/conc.$tag img001-304.448 delout/ndel.$tag > delout/nout.$tag
  hdftor8 ../19${yy}/${tag}.TSN 
  ./dels ${base}.s/conc.$tag img001-316.332 delout/sdel.$tag > delout/sout.$tag

  rm img* 

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
