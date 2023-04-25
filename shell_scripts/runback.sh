tag=20120405

while [ $tag -le 20120410 ]
do

  ./getfiles.sh $tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done  
