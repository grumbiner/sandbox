for fn in glob_Azov.areas glob_Baffin.areas glob_Baltic.areas glob_Bering.areas glob_Black.areas glob_Hudson.areas glob_Japan.areas glob_Labrador.areas glob_Maine.areas glob_Ob.areas glob_Okhotsk.areas glob_White.areas glob_Yellow.areas
do
  base=`echo $fn | cut -f1 -d\. | cut -f2 -d\_`
  ref=$base
  ./stats ../subarctic.analyses/glob_${base}.areas ../subarctic.analyses/glob_${ref}.areas > ${base}.$ref.deannual
done
