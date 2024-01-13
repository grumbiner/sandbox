#!/usr/bin/perl

while ($line = <STDIN> ) {
  chop($line);
  if ($line =~ /user.(\S{3,8}) by/ ) {
    #print $1,"\n";
    $names{$1} += 1;
  }
}

while ( ($key, $value) = each(%names) ) {
  print $key," ",$value,"\n";
}

