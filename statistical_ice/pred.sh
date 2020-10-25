wgrib rtg_sst.2002 | grep 021001

wgrib rtg_sst.2002 | grep 021001 | wgrib -i -nh rtg_sst.2002 -o sst
wgrib npsg.2002 | grep 021001 | wgrib -i -nh npsg.2002 -o ice
./a.out ice avg.1001 sst a365 out
