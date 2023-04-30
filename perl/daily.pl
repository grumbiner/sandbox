#!/usr/bin/perl

while ($line = <STDIN> ) {
  @words=split(/;/, $line);
  printf("%7.3f %7.3f %s",$words[0], $words[1], $words[$#words]);
  #printf("%7.3f %7.3f\n",$words[0], $words[1]);
}  
