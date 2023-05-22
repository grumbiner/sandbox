
set -xe

tag=`date +"%y%m%d"`
tag=`expr $tag - 1`
tag=`dtgfix3 $tag`

assign -a /cray3_com/fnl/PROD/sst.$tag/sst.T12Z.eta_grid -Fcos -Nibm fort.10
assign -a sst.$tag.grib -s unblocked fort.12

echo $tag | ./sst14b

