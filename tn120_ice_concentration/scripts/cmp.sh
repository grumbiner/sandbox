for mo in 01 02 03 04 05 06 07 08 09 10 11 12
do
  if [ -f npsg.01${mo} -a -f npsg.2001${mo} ] ; then
    cmp npsg.01${mo} npsg.2001${mo}
    if [ $? -eq 0 ] ; then
      echo rm npsg.01${mo}
    fi
  fi
done
