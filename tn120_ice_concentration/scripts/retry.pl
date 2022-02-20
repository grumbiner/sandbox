#!/usr/bin/perl

#Version to rerun with dates that never produced a good data file

while ($line = <STDIN>) {
  chop($line);
  @words=split(/\s+/,$line);
  $dtgroup = $words[0];
  $pole = $words[$#words];
  print $dtgroup," ",$pole,"\n";

  system "./piece.sh $dtgroup $pole";

}
