pole=south
export pole

base=9612
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 \
          23 24 25 26 27 28 29 30 31
do
  od=/dm/wd21rg/$pole/

  tar xvf ${od}/${base}$dy/t${base}$dy.$pole.tar

  for hh in 12 24 36 48 60 72 84 96 108 120 132 144 156 168
  do 

    tar xvf f${hh}.tar

    assign -a conc fort.10
    assign -a thick fort.11
    assign -a hml  fort.12
    assign -a atm.flux fort.13
    assign -a oce.flux fort.14
    assign -a sml fort.15
    assign -a tml fort.16
    assign -a vels fort.20
    assign -a $HOME2/gribout/${pole}grib.${base}$dy.f$hh -s unblocked fort.51
  
    echo $base | cut -c1-2 > gin
    echo $base | cut -c3-4 >> gin
    echo $dy >> gin
    echo $hh >> gin
    echo 12 >> gin

    cat gin | $HOME2/execs/sicegrib

  done

done
