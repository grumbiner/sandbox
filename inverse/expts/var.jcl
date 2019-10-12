#QSUB -lT 2000
#QSUB -lM   2Mw
#QSUB -me
#QSUB -o /dm/wd21rg/icevar.out3

cd /ptmp1/wd21rg
echo freezout.aug > v2in 

set -xe
ja
  $HOME3/execs/icevary < $HOME3/inverse/arry/vin
  $HOME3/execs/vremap < v2in 
ja -chlst
