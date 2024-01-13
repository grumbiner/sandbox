#QSUB -lT  150
#QSUB -lM    1Mw
#QSUB -me

cd $HOME1
if [ ! -d fluxdir ] ; then
  mkdir fluxdir
fi
cd fluxdir

arcdir=$yz/tarz
export arcdir

#Create the meteorological files:
yy=95
mo=01
dy=23 # start number a day early
export yy mo dy

fday=0
while [ $fday -lt  9 ] 
do

  fday=`expr $fday + 1 `

#fix months, days, years here
    dy=`expr $dy + 1`
    dtg=`dtgfix $yy $mo $dy `
    yy=`echo $dtg | cut -c1-2`
    mo=`echo $dtg | cut -c3-4`
    dy=`echo $dtg | cut -c5-6`
    
#Extract the flux files
#    $yz/bin/tarxvf  ${arcdir}/gz${yy}${mo}${dy} \
    $yz/bin/tarxvf  gz${yy}${mo}${dy} \
        crsflx.${yy}${mo}${dy}00 \
        crsflx.${yy}${mo}${dy}06 \
        crsflx.${yy}${mo}${dy}18 \
        crsflx.${yy}${mo}${dy}12
done
