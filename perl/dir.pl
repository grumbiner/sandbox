#!/usr/bin/perl

#  737.9 Mb of file /home/ftp/pub/history/ice/climatology/north/nhallmonths.grib
#  686.6 Mb of file /home/ftp/pub/fog/t12z.fvnhg
#  668.6 Mb of file /home/ftp/pub/fog/t00z.fvnhg
#  187.0 Mb of file /home/ftp/pub/history/ice/climatology/north/varclimo.grib
#  187.0 Mb of file /home/ftp/pub/history/ice/climatology/north/annualclimo.grib
#  170.6 Mb of file /home/ftp/pub/history/ice/climatology/south/varclimo.grib
#  170.6 Mb of file /home/ftp/pub/history/ice/climatology/south/avgclimo.grib
#  121.5 Mb of file /home/ftp/pub/vessel.icing/t00z.siceg_1x1.all
#  119.0 Mb of file /home/ftp/pub/cdas/archive/ice.1996.Z
#  108.2 Mb of file /home/ftp/pub/cdas/eng.20020731

while ( $line = <STDIN> ) {
  chop ($line);
  @full = split(/\s+/,$line);
  @path = split(/\//,$full[$#full]) ;

  $name = $path[4];
  for ($i = 5; $i < $#path ; $i++) {
    $name = $name . "/" . $path[$i] ;
  }
  $sizes{$name} += $full[1];

  $name = $path[4];
  for ($i = 5; $i < $#path-1 ; $i++) {
    $name = $name . "/" . $path[$i] ;
  }
  $s2{$name} += $full[1];

}

while ( ($key, $value) = each(%sizes) ) {
  printf "%8.1f Mb from %s\n",$value, $key;
}
print "zzzzzzzzzzzzzzzzzzzzzzz\n";

while ( ($key, $value) = each(%s2) ) {
  printf "%8.1f Mb from %s\n",$value, $key;
}
