od=/eldisk0/wd21rg/north/961001

assign -a $od/conc fort.10
assign -a $od/thick fort.11
assign -a $od/hml  fort.12
assign -a $od/atm.flux fort.13
assign -a $od/oce.flux fort.14
assign -a $od/sml fort.15
assign -a $od/tml fort.16
assign -a $od/vels fort.20
assign -a gribout -s unblocked fort.51

echo 96 > gin
echo 10 >> gin
echo 01 >> gin
echo 12 >> gin
echo 12 >> gin

cat gin | ./nicegrib

