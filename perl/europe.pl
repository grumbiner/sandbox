#!/usr/bin/perl

$i = 0;
while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);
#  print $words[1],"\n";
#  print $words[2],"\n";
  if ( ($words[1] > 0) && ($words[2] > 0) ) {
    print $line;
    $i += 1;
  }
}
print $i," points in box\n";
