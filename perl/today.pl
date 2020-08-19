#!/usr/bin/perl
#
while ($line = <STDIN>) {
  if ($line =~ /(.*)TODAY(.*)/ ) {
    #print $1,"hello ",$2;
    print $1,$ARGV[0]," ",$ARGV[1]," ",$ARGV[2] ,$2;
  }
  else {
    print $line;
  }
}
