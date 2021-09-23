#!/usr/bin/perl

open in1, "<$ARGV[0]";
open in2, "<$ARGV[1]";
open in3, "<$ARGV[2]";
open in4, "<$ARGV[3]";
open in5, "<$ARGV[4]";

while ($line = <in1>) {
   #Solar
  @words=split(/\s+/,$line);
  $sc = $words[3];

  $line = <in2>;
  @words = split(/\s+/,$line);
  #print $line;
  $a2 = $words[3];

  $line = <in3>;
  @words = split(/\s+/,$line);
  $a3 = $words[3];

  $line = <in4>;
  @words = split(/\s+/,$line);
  $a4 = $words[3];

  $line = <in5>;
  @words = split(/\s+/,$line);
  $a5 = $words[3];

  print $sc, "," ,$a2, "," ,$a3, "," ,$a4, "," ,$a5, "\n";
}
