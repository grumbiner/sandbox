#!/usr/bin/perl

for ($i = 0; $i < 144; $i++) {
  for ($j = 0; $j < 18; $j++) {
    while ($line = <STDIN>) {
      chop($line);
      @words = split(/\s+/, $line);
    }
  }
}
  






if ($line >= 0 ) {
  print "greater\n";
}
if ($line < 0 ) {
  print "less\n";
}
print "None of the above\n";

