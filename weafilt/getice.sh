for mo in 01 02 03 04 05 06 07 08 09 10 11 12
do
  wgrib ice5min.2005${mo} | grep 05${mo}01 | wgrib -i ice5min.2005${mo} -nh -o ice.${mo}01
done
