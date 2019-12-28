set -x
cd $HOME2/for.cofs

cp $HOME41/cfs.data/* .
cp $HOME41/cpp/qc3  .
cp $HOME2/for.cofs/august.avg $HOME2/for.cofs/long.970731

base=9708
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 28 29 30 31
do

  dm1=`expr ${base}$dy - 1`
  dm1=`dtgfix3 $dm1`
  cp $HOME2/for.cofs/long.$dm1 qc.$dm1
  time ./qc3 qc.$dm1 long.$dm1 short.$dm1 tsout.${base}$dy \
                   long.${base}$dy short.${base}$dy 
  mv *.${base}${dy}.asc /marine/misc/cofs

done

