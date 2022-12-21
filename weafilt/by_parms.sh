for i in 0 1 2 3 4 5 6
do
  for j in 1 2 3 4 5 6
  do
    if [ $j -gt $i ] ; then
      grep " c1 c2 $i $j " binary | ./binobj.pl > bin2obj.$i.$j
    fi
  done
done
cat unary | ./multiobj.pl > unary.out   
