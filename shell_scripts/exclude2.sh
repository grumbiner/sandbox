fn=$1
for s in pub.data1.nccf.com.arkv pub.emc.gc nco.nwprod gdas1.t..z.sanl gdas1.t..z. '\/var\/ftp\.' gdas1.t..z.radstat data2.JTWC.gfs gdas1.forecmwf  
do
  grep -v $s $fn > a
  mv a $fn
done
