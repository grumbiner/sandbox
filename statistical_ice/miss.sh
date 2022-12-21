#!/bin/ksh

#Third pass: Fill in missing/invalid data from adjacent days and
#  see how many points unfilled that leaves
#Robert Grumbine 17 September 2004

tagm=19950920
tag=19950921
tagp=19950922
while [ $tagp -le 20021031 ] 
do
  if [ -f filtice.$tagm -a -f filtice.$tag -a -f filtice.$tagp ] ; then
    ./missfill filtice.$tagm filtice.$tag filtice.$tagp fill1.$tag >> missout
  fi

  tagm=$tag
  tag=$tagp
  tagp=`expr $tagp + 1`
  tagp=`dtgfix3 $tagp`
done
