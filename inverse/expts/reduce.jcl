echo 2 > rin
echo 5 >> rin
tag=960101

while [ $tag -le 960430 ]
do
  ~seaice/execs/reducer /export/s2/reanl/ice/umasknorth.$tag /export/s2/reanl/ice/umasksouth.$tag n.$tag s.$tag < rin

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

