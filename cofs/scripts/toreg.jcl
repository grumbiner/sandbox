#QSUB -lT 90
#QSUB -lM 8Mw

mkdir -p /tmp/wd21rg/cpp2
#cp -aur /eldisk0/wd21rg/cpp2/* .

cd /tmp/wd21rg/cpp2
time ./toreg short.971204.asc short.bin.reg > alpha
flowview -Luchm -O 99 > report.1
cp alpha report.1 /eldisk0/wd21rg/cpp2/

