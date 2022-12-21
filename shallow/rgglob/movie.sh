i=0
while [ $i -le 36525 ] 
do
  if [ $i -lt 10 ] ; then
    i=000$i
  elif [ $i -lt 100 ] ; then
    i=00$i
  elif [ $i -lt 1000 ] ; then
    i=0$i
  fi
  convert nh${i}.xpm nh${i}.gif
  i=`expr $i + 25`
done
ls -1 nh????.gif | sort > a
ls -1 nh?????.gif | sort >> a
whirlgif -o anim.gif -i a
