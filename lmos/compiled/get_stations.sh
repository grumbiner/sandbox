k=1
while [ $k -le 2999 ]
do
  ./gfs_obs $k > a$k
  k=`expr $k + 1`
done
find . -name 'a[0-9]*' -size -25k
