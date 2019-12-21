#!/usr/bin/perl

#Trim a file to unique addresses and write those out

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
#  print $words[0], ' ', $words[1],' ',  $words[2],"\n";
  $addrcount{$words[1]} += 1;
}

while ( ($key, $value) = each(%addrcount) ) {
  print $key,"\n";
}
