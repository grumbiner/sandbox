for sep in 100 400 200 600 800 300 500
do
  g++ -O2 genecluster.C -o dist$sep -DSIMIL=$sep -DPOPULATION=250
  for srand in 1 2 3 4 5 6 7 8 9 10
  do
    time ./dist$sep $srand > sep$sep.$srand
  done
done
