#!/usr/bin/perl

while ($line=<STDIN>) {
  chop($line);
  @words=split(/ +/,$line);
  $names{$words[$#words -1]} += 1;
}
while ( ($key,$value) = each(%names) ) {
  print $value," problems for buoy ",$key,"\n";
}


