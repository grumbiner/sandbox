yy=1985
while [ $yy -le 2009 ]
do

  if [ `expr $yy % 4` -eq 0 ] ; then
    ./leap fout.${yy}??.bin fout.${yy}.bin > txt.${yy}
  else 
    ./splice fout.${yy}??.bin fout.${yy}.bin > txt.${yy}
  fi

  yy=`expr $yy + 1`
done
