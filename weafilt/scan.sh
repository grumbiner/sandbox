for p in a b c d
do
  time ./scan match2.3$p > $p
  cat $p | ./multiobj.pl > $p.out
  grep 'ice tc' $p.out > $p.ice
  grep 'water tc' $p.out > $p.water
done

