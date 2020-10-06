for mo in 01 02 03 05 06 07 08 09 10 11 12
do
  time ./splice2 seaice_gland5min ice.${mo}01 ../arc/out.$mo match3.${mo}01
done
