g++ -Wall -O2 -DLINUX cmp.C -o cmp /usr/local/lib/libcppf
g++ -Wall -O2 -DLINUX show.C -o show /usr/local/lib/libcppf
for base in oldicing newicing oldfog newfog
do
  wgrib ${base} | wgrib -i ${base} -nh -o ${base}.bin
done

./cmp oldicing.bin newicing.bin > icing.out
mkdir icing; mv *.xpm icing.* icing

./cmp oldfog.bin newfog.bin > fog.out
mkdir fog; mv *.xpm fog.* fog
