#grep '[0-9] 3   1 ' alpha >
for lead in 2 3 4 5 6 
do
  grep "[0-9] 3   $lead " alpha > lead.$lead
done
for lead in 12 24 36 48 60 72 84 96 
do
  grep "[0-9] 3  $lead " alpha > lead.$lead
done
for lead in 108 120 
do
  grep "[0-9] 3 $lead " alpha > lead.$lead
done
