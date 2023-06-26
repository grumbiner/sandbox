tmp=`echo $1 | cut -f2-90 -d\/`
base=`echo $tmp | cut -f1-2 -d\.`
convert -flip $1 $base.gif
