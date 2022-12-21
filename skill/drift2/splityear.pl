#!/usr/bin/perl

$maxsp = 45.0;
$minsp = 0.25;

open f93, ">93";
open f94, ">94";
open f95, ">95";
open f96, ">96";
open f97, ">97";
open f98, ">98";
open f99, ">99";
open f100, ">100";

while ($line=<STDIN>) {
  chop($line);
  @stats=split(/ +/,$line);
  if ($stats[4] > 0) {
    $rate = $stats[10] / $stats[4];
    if ($rate < $maxsp && $rate > $minsp ) {
      if ($stats[0] == 93) {
        print f93 $line,"\n";
      }
      if ($stats[0] == 94) {
        print f94 $line,"\n";
      }
      if ($stats[0] == 95) {
        print f95 $line,"\n";
      }
      if ($stats[0] == 96) {
        print f96 $line,"\n";
      }
      if ($stats[0] == 97) {
        print f97 $line,"\n";
      }
      if ($stats[0] == 98) {
        print f98 $line,"\n";
      }
      if ($stats[0] == 99) {
        print f99 $line,"\n";
      }
      if ($stats[0] == 100) {
        print f100 $line,"\n";
      }

    } 
  } 

}
