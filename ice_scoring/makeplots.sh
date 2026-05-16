#!/bin/sh
#Make plots of model skill for multiple skill measures at 'all' lead times
#Robert Grumbine

tag=20171017
#while [ $tag -le 20180401 ]
#do
#done

#for region in global(need to update program for this) nhlevel shlevel
for region in nhlevel shlevel 
do
  for lead in 000 024 048 072 096 120 144 168 192
  do
    grep $region scoreout.*.$lead > ${region}.$lead
    level=00
    while [ $level -le 95 ]
    do
      grep " 0.$level " $region.$lead > $region.$lead.$level

      echo set grid > plot.$region.$lead.$level
      echo set title \"$region Scores for RTOFS-G $region \@ $level critical level and $lead hours\" >> plot.$region.$lead.$level
      echo set xlabel \"Forecast \# from 17 October 2017\" >> plot.$region.$lead.$level
      echo >> plot.$region.$lead.$level 
      term=postscript 
      ft=ps 
      echo set term $term >> plot.$region.$lead.$level
      echo set out \"$region.$lead.$level.$ft\" >> plot.$region.$lead.$level
      echo >> plot.$region.$lead.$level 
if [ $level -le 50 ] ; then
      echo set yrange [0.86:1.10] >> plot.$region.$lead.$level
      echo set ytic 0.02        >> plot.$region.$lead.$level
else
      if [ $level -le 75 ] ; then
        echo set yrange [0.80:1.25] >> plot.$region.$lead.$level
        echo set ytic 0.05        >> plot.$region.$lead.$level
      else
        echo set yrange [0.5:1.5] >> plot.$region.$lead.$level
        echo set ytic 0.10        >> plot.$region.$lead.$level
      fi
fi
      echo >> plot.$region.$lead.$level 
      echo plot \"$region.$lead.$level\" using 12 title \"Bias\" with lp, \"$region.$lead.$level\" using 11 title \"CSI\" with lp, \"$region.$lead.$level\" using 10 title \"%Correct\" with lp, \"$region.$lead.$level\" using 7 title \"PoD\" with lp >> plot.$region.$lead.$level
#set out nh.far.fcr.$lead.$level.$ft
#set yrange [0:0.25]
#set ytic 0.05
#plot (using 8 - far, 9 - fcr)

      gnuplot plot.$region.$lead.$level
      convert -rotate 90 $region.$lead.$level.$ft $region.$lead.$level.png
#convert fcr version too 

      level=`expr $level + 5`
      if [ $level -lt 10 ] ; then
        level=0$level
      fi
    done
  done
done

