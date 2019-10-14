cd /ptmp1/wd21rg

# For the remapping section, assign the files for the daily maps.
i=10
j=100
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 
do
  for hh in 00 12 
  do
    i=`expr $i + 1`
    assign -a vara.9408${dy}${hh} -s unblocked fort.$i
    j=`expr $j + 1`
    assign -a varr0.9408${dy}${hh} -s unblocked fort.$j
  done
done

echo freezout.aug > hin
ja
  $HOME3/inverse/arry/vremap < hin
ja -chlst
