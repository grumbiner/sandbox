#!/usr/bin/perl
# 7 March 2014  Robert Grumbine

$maxsp = 45.0;
$minsp = 0.25;

open f98, ">98";
open f99, ">99";
open f100, ">100";
open f101, ">101";
open f102, ">102";
open f103, ">103";
open f104, ">104";
open f105, ">105";
open f106, ">106";
open f107, ">107";
open f108, ">108";
open f109, ">109";
open f110, ">110";
open f111, ">111";
open f112, ">112";
open f113, ">113";
open f114, ">114";
open f115, ">115";

while ($line=<STDIN>) {
  chop($line);
  @stats=split(/ +/,$line);
  if ($stats[4] > 0) {
    $rate = $stats[10] / $stats[4];
    if ($rate < $maxsp && $rate > $minsp ) {
      if ($stats[0] == 98) {
        print f98 $line,"\n";
      }
      if ($stats[0] == 99) {
        print f99 $line,"\n";
      }
      if ($stats[0] == 100) {
        print f100 $line,"\n";
      }
      if ($stats[0] == 101) {
        print f101 $line,"\n";
      }
      if ($stats[0] == 102) {
        print f102 $line,"\n";
      }
      if ($stats[0] == 103) {
        print f103 $line,"\n";
      }
      if ($stats[0] == 104) {
        print f104 $line,"\n";
      }
      if ($stats[0] == 105) {
        print f105 $line,"\n";
      }
      if ($stats[0] == 106) {
        print f106 $line,"\n";
      }
      if ($stats[0] == 107) {
        print f107 $line,"\n";
      }
      if ($stats[0] == 108) {
        print f108 $line,"\n";
      }
      if ($stats[0] == 109) {
        print f109 $line,"\n";
      }
      if ($stats[0] == 110) {
        print f110 $line,"\n";
      }
      if ($stats[0] == 111) {
        print f111 $line,"\n";
      }
      if ($stats[0] == 112) {
        print f112 $line,"\n";
      }
      if ($stats[0] == 113) {
        print f113 $line,"\n";
      }
      if ($stats[0] == 114) {
        print f114 $line,"\n";
      }

    } 
  } 

}
