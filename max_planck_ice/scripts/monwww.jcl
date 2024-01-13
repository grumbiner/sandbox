#arg1 is yymm to process
BASEDIR=/data/forecasts/new/icetest
EXDIR=/home/wd21rg/icemodel/execs

set -x

for pole in north south
do
  for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
            21 22 23 24 25 26 27 28 29 30 31
  do
    for hr in 00 06 12 18
    do
       if [ -f ${BASEDIR}/${pole}/conc.${pole}.${1}${dy}$hr ] ; then
         ${EXDIR}/${pole}xpm ${BASEDIR}/${pole}/conc.${pole}.${1}${dy}$hr \
                             /home/wd21rg/icemodel/running/${pole}/bathy.$pole \
                             ${pole}${dy}${hr}.xpm   100.0 0.0 
         xpmtogif ${pole}${dy}${hr}.xpm
       fi
    done
  done
  ls -1 ${pole}[0-3]???.gif > $pole
  whirlgif -o ${pole}${1}.gif -i ${pole}
done


