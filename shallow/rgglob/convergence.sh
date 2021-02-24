for t in 300 240 180 150 120 100 75 60 
do
  time ./rgglob $t > ${t}.txt
  rm *.xpm
done
for t in 50 30 25 20 15 12 10 6 5 4 3 2 1
do
  time ./rgglob $t > ${t}.txt
  rm *.xpm
done
