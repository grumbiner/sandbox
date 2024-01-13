#!/usr/bin/perl

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/,$line);
  print $words[$#words]," ",$words[$#words -1]," ",$words[$#words - 2], " ",-$words[$#words - 2]/$words[$#words -1],"\n";
}
