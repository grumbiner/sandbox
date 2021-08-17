#!/usr/bin/perl

while ( $line = <STDIN> ) {
  chop ($line);
  @full = split(/\s+/,$line);
  @path = split(/\//,$full[$#full-9]) ;

  if ( $full[$#full-6] =~ /o/ ) {
    $sizes{$path[$#path]} += $full[$#full - 10];
    $counts{$path[$#path]} += 1;
    $avgs{ $full[$#full - 10]} += 1;
  }

}

while ( ($key, $value) = each(%sizes) ) {
  printf "%8.1f Mb %5d times, avg size %7.3f Mb, of %s\n",$value / 1024 / 1024, $counts{$key}, $value / 1024 / 1024/ $counts{$key}, $key;
}
print "zzzzzzz\n";
while ( ($key, $value) = each(%avgs) ) {
  printf "%5d files of size %d\n",$value, $key;
}

