#!/bin/ksh

#Second pass: Ensure that all days have data by interpolation
#Robert Grumbine 17 September 2004

tagm=19950920
tag=19950921
tagp=19950922
while [ $tagp -le 20021031 ] 
do
  if [ ! -f filtice.$tag ] ; then
    #1 day gap:
    if [ -f filtice.$tagm -a -f filtice.$tagp ] ; then
      ./time_interpf filtice.$tagm filtice.$tagp 1
      mv out.00 filtice.$tag
    fi
    #n day gap
    k=1
    tagn=`expr $tagp + 1`
    tagn=`dtgfix3 $tagn`
    while [ ! -f filtice.$tagn ] 
    do
      tagn=`expr $tagp + 1`
      tagn=`dtgfix3 $tagn`
      k=`expr $k + 1`
    done
    echo k = $k
    ./time_interpf filtice.$tagm filtice.$tagn $k
    l=0
    tagk=$tag
    while [ $l -lt $k ]
    do
      mv out.0$l filtice.$tagk
      tagk=`expr $tagk + 1`
      tagk=`dtgfix3 $tagk`
      l=`expr $l + 1`
    done 
  fi

  tagm=$tag
  tag=$tagp
  tagp=`expr $tagp + 1`
  tagp=`dtgfix3 $tagp`
done
