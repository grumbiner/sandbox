
yy=2005
while [ $yy -le 2013 ]
do
  touch -t ${yy}12312359 a
  if [ ! -d ../rglib.$yy ] ; then
    mkdir ../rglib.$yy
  fi
  find . -maxdepth 1 ! -newer a -exec ifm {} ../rglib.$yy \;

  yy=`expr $yy + 1`
done 
