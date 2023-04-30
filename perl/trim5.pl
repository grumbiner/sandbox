#!/usr/bin/perl

$toler = 5;
$dist_toler = 20.0;

while ($line = <STDIN>) {
  @words = split(/\s+/,$line);
  $n = $#words - 1;
  if ( (abs($words[$n]) > $toler) && ($words[$n+1] > $dist_toler) ) {
    print $line;
  }
}
