#!/bin/ksh
#Regionalized analyses
#Robert Grumbine March 2005

#Version computing with min_conc as argument
tag=20000225
while [ $tag -le 20040531 ]
do
  yy=`echo $tag | cut -c1-4`

  ./seas.x ${yy}/umasknorth12.$tag 15 > nh_area.$tag
  ./glob globs/fill5min.$tag 15 > glob_area.$tag

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

#Making time series
for name in Hudson Okhotsk Bering Baffin Labrador St_Law Baltic Azov Ob White Japan Yellow Black Maine Chesapeake Deleware
do
  grep $name nh_area.* > nh_$name
  cat nh_$name | cut -c41-61 > nh_$name.areas
  ./series nh_$name.areas > nh_$name.acor
  
  grep $name glob_area.* > glob_$name
  cat glob_$name | cut -c43-63 > glob_$name.areas
  ./series glob_$name.areas > glob_$name.acor
done


#Cross-prediction-1
for fn in glob_Azov.areas glob_Baffin.areas glob_Baltic.areas glob_Bering.areas glob_Black.areas glob_Hudson.areas glob_Japan.areas glob_Labrador.areas glob_Maine.areas glob_Ob.areas glob_Okhotsk.areas glob_White.areas glob_Yellow.areas
do
  base=`echo $fn | cut -f1 -d\. | cut -f2 -d\_`
  for fn2 in glob_Azov.areas glob_Baffin.areas glob_Baltic.areas glob_Bering.areas glob_Black.areas glob_Hudson.areas glob_Japan.areas glob_Labrador.areas glob_Maine.areas glob_Ob.areas glob_Okhotsk.areas glob_White.areas glob_Yellow.areas
  do
    ref=`echo $fn2 | cut -f1 -d\. | cut -f2 -d\_`
    if [ $ref != $base ] ; then
      ./cross glob_${base}.areas glob_${ref}.areas > ${base}.$ref
    fi
  done
done

#cross-prediction
for fn in glob_Azov.areas glob_Baffin.areas glob_Baltic.areas glob_Bering.areas glob_Black.areas glob_Hudson.areas glob_Japan.areas glob_Labrador.areas glob_Maine.areas glob_Ob.areas glob_Okhotsk.areas glob_White.areas glob_Yellow.areas
do
  base=`echo $fn | cut -f1 -d\. | cut -f2 -d\_`
  for fn2 in glob_Azov.areas glob_Baffin.areas glob_Baltic.areas glob_Bering.areas glob_Black.areas glob_Hudson.areas glob_Japan.areas glob_Labrador.areas glob_Maine.areas glob_Ob.areas glob_Okhotsk.areas glob_White.areas glob_Yellow.areas
  do
    ref=`echo $fn2 | cut -f1 -d\. | cut -f2 -d\_`
    if [ $ref != $base ] ; then
      ./stats glob_${base}.areas glob_${ref}.areas > ${base}.$ref.deannual
    fi
  done
done

#Self-prediction
for fn in glob_Azov.areas glob_Baffin.areas glob_Baltic.areas glob_Bering.areas glob_Black.areas glob_Hudson.areas glob_Japan.areas glob_Labrador.areas glob_Maine.areas glob_Ob.areas glob_Okhotsk.areas glob_White.areas glob_Yellow.areas
do
  base=`echo $fn | cut -f1 -d\. | cut -f2 -d\_`
  ref=$base
  ./stats ../subarctic.analyses/glob_${base}.areas ../subarctic.analyses/glob_${ref}.areas > ${base}.$ref.deannual
done

