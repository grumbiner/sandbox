#Construct the traditional climatological average by days:

#1984 is an arbitrary year -- has leap is the only importance

tag=19840101
while [ $tag -le 19841231 ]
do
  base=`echo $tag | cut -c5-8`
  ./tradavg trad_day.$base  /usr2/rmg3/reysst/reformed/qdoi.*$base

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

#Now construct the months' averages:
for mo in 01 02 03 04 05 06 07 08 09 10 11 12
do
  ./tradavg trad_mon.$mo  /usr2/rmg3/reysst/reformed/qdoi.????${mo}??
done

