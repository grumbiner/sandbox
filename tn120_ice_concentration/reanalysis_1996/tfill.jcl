tag=940419

i=0
if [ -f fill.in ] ; then
  rm fill.in
fi
touch fill.in

while [ $i -lt 173 ] 
do
  i=`expr $i + 1`
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  base=`echo $tag | cut -c1-4`
  dy=`echo $tag | cut -c5-6`
  if [ ! -d fin.$base ] ; then
    mkdir fin.$base
  fi
  echo ice.${base}/iceout.$tag >> fill.in
  echo fin.${base}/fin.$tag    >> fill.in
done

./fill < fill.in
