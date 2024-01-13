#!/usr/bin/perl

while ($line = <STDIN> ) {
  chop($line);
  @words = split(/\s+/,$line);
  @level = split(/\//,$words[$#words]);
  print $#level,",",$words[0],",",$words[1],"\n";
}
