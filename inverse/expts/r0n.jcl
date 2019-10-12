cd /ptmp1/wd21rg
set -xe

base=9408
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 28 29
do

  tag=${base}${dy}

  $HOME3/inverse/overhead/r0n $DHOME/ices/anrev.$tag $DHOME/grow/growout.north$tag r0n.$tag


done

