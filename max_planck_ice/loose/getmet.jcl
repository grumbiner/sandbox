tag=`date +'%y%m%d`
BASE=/com/mrf/prod/mrf.$tag/

for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168
do
  rcp cray3:$BASE/drfmr.T00Z.SFLUXGrbF$time /export/s2/mrf
done

