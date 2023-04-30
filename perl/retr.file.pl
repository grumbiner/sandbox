#!/usr/bin/perl

#  750 retrievals of /pub/waves/latest_run/wna.41009.bull

while ( $line = <STDIN> ) {
  chop ($line);
  @full = split(/\s+/,$line);
  @path = split(/\//,$full[$#full]) ;
  $fn = $path[$#path];
  $counts{$fn} += $full[1];
  #print $full[1]," ",$fn,"\n"
}

while ( ($key, $value) = each(%counts) ) {
  printf "%5d times of %s\n",$value, $key;
}

