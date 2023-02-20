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
  @path = split(/\//,$full[$#full-9]) ;

  if ( $full[$#full-6] =~ /o/ ) {
    $sizes{$path[$#path]} += $full[$#full - 10];
    $counts{$path[$#path]} += 1;
  }

}

while ( ($key, $value) = each(%sizes) ) {
  printf "%8.1f Mb %5d times of %s\n",$value / 1024 / 1024, $counts{$key}, $key;
}
print "zzzzzzzzzzzzzzzzzzzzzzz\n";

