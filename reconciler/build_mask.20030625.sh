make refill refill.tries bathy toab paving

if [ ! -f fout ] ; then
  time ./refill shorelines/gshhs_f.b fout a.xpm > out1 2> out2 

  #time ./refill.tries shorelines/try50.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try25.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try15.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try8.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try4.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try1.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try0.4.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try0.2.b fout a.xpm > out1 2> out2 
  #time ./refill.tries shorelines/try0.0625.b fout a.xpm > out1 2> out2 
fi

if [ $? -eq 0  -a -f fout -a ! -f bathyout ] ; then
  ./bathy fout etopo2/etopo2.raw bathyout bath.xpm 0 0 > hycom.bathy.out
fi

if [ -f bathyout ] ; then
  ./paving bathyout 40.0 330.0 paveout paver/straits
# ./paving bathyout lat lon output_file [straits]
fi 

if [ -f paveout ] ; then
  ./toab paveout paved.a paved.b
fi
