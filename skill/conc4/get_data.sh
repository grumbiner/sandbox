start=20150217
finish=20150227

export PDY=$start
PDYm1=`expr $start - 1`
PDYm1=`dtgfix3 $PDYm1`
export PDYm1
export tag=$PDY

while [ $tag -le $finish ]
do
  time ./sice.sh
  time ./gfs.sh
  time ./gdas.sh
  time ../../cfs/cfs.sh

  PDYm1=$PDY
  PDY=`expr $PDY + 1`
  PDY=`dtgfix3 $PDY`
  tag=$PDY
done
