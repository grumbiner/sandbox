# For the remapping section, assign the files for the daily maps.
i=10
j=100
for dy in 0915 0916 0917 0918 0919 0920 0921 0922 0923 0924 \
          0925 0926 0927 0928 0929 0930 1001 1002 1003 1004 \
          1005 1006 1007 1008 1009 1010 1011 1012 1013 1014
do
for hh in 00 12 
do
  i=`expr $i + 1`
  assign -a vara.94${dy}${hh} -s unblocked fort.$i
  j=`expr $j + 1`
  assign -a varr0.94${dy}${hh} -s unblocked fort.$j
done

echo vary.out > hin
ja
  vremap < hin
ja -chlst
