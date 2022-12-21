yr=full
yy=98
  time ./buoycheck 50.0 3.0 6 checked.$yr > comments.$yr
  ./avg 50.0 3.0 6 checked.$yr fout1.${yr} fout2.${yr}
  ./score fout2.$yr 6 $yy > scores.$yr
