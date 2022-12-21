for mo in 01 02
do
  for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 \
            20 21 22 23 24 25 26 27 28 29 30 31
  do
    if [ -f sk2.2000${mo}$dy ] ; then
      cp sk2.2000${mo}$dy sk2.100${mo}$dy
    fi
  done
done
