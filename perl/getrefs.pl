#!/usr/bin/perl

#Script to search out and collect e-mail addresses from
#  mail
#-- variant to come: pull up URLs
$keyword="http://";

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  foreach $w (@words) {
     #For now we'll call it an address if it has an @.  Much to be cleared out. 
     if ($w =~ /$keyword/) {
         @parts = split(/$keyword/, $w);
         $parts[0] = &clearup($parts[0]);
         $parts[1] = &clearup($parts[1]);
         $addrs{$w} += 1;
     }
   }
}

while ( ($key, $value) = each(%addrs) ) {
  print $value," ", $key," \n";
}


sub clearup {
  local $tmp;
  $_[0] =~ s/[,;<>()"'`]+//;
  return $_[0];
}
