cd /ptmp1/wd21rg
set -xe

base=9412
for dy in 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
do
  tag=${base}${dy} 
  tar -xvf filt.$tag 
  $HOME3/inverse/overhead/r0 revi.$tag growout r0.$tag
  mv growout growout.$tag
  tar cvf filt2.$tag r0.$tag revi.$tag growout.$tag delta.$tag oneout.$tag
  rm r0.$tag filt.$tag
done

