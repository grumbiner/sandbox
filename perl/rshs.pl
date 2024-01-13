#!/usr/bin/perl

while ( $line = <STDIN> ) {
  chop($line);
  if ($line =~ /polar rsh/) {
  if ($line =~ /session opened for user (\S+)/) {
    $user{$1} += 1;
  }
  }
}
while ( ($key, $value) = each(%user) ) {
  print $value," ",$key,"\n";
}

