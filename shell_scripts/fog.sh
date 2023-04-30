i=1
while [ $i -le 56 ]
do
  for l in del ops test
  do
    convert -flip ${l}${i}.xpm ${l}${i}.gif
  done
  i=`expr $i + 1`
done 
