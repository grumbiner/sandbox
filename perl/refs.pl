#!/usr/bin/perl
#
while ($line = <STDIN>) {
  if ($line =~ /http:\/\/(\S+)/) {
    #print "http://",$1,"\n";
    $refs{$1} += 1;
  }
}
while ( ($key, $value) = each(%refs) ) {
  print $value," ",$key,"\n";
}
