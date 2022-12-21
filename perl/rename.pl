#!/usr/bin/perl
#
#nh12.20170915.gif

while ($line = <STDIN>) {
  chop $line;
  if ($line =~ /seaice.t00z.(\S+).gif.(\d{8})/) {
    print "cp -p ".$line."  ".$1.".".$2.".gif","\n";
  }
}
