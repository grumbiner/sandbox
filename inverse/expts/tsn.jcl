cd /ptmp1/wd21rg


echo 28 > tin5
for tag in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
           21 22 23 24 25 26 27 28
do

  echo $DHOME/icerev/anrev.9408$tag >> tin5
  echo r0n.9408$tag >> tin5
done
 
time $HOME3/inverse/overhead/tsn tin5 tout5
