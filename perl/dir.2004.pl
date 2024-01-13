#!/usr/bin/perl

#omb, mmab, waves, seaice, winds, sst

while ( $line = <STDIN> ) {
  chop ($line);
  @full = split(/\s+/,$line);
  @path = split(/\//,$full[$#full]) ;
  $n    = $full[$#full-1];
  #print $#full,"\n";

  $name="";
  for ($i = 1; $i < $#path ; $i++) {
    $name = $name . "/" . $path[$i];
  }
  #print $line," ",$n," ",$name,"\n";

  $counts{$name} += $n;

}

while ( ($key, $value) = each(%counts) ) {
  printf "%8d of %s\n",$value, $key;
}
