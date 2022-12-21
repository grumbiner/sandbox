#!/usr/bin/perl

#Robert Grumbine
#27 January 2016
#
#Collect the analysis ideas from the 2009-2010 qclimits work
#
$t6v  = 7;
$t7v  = 12;
$t10v = 17;
$t19h = 21;
$t19v = 22;
$t24v = 27;
$t37v = 32;

$lat = 1;
$lon = 2;
$icecon = 3;
$sst    = 4;

$g2419 = 29;
$g3719 = 34;
$g3724 = 35;


$GR2419LIM = 0.065; # RG
$GR3719LIM = 0.085; # RG
$GR3724LIM = 0.045; # RG

$filtered = 0;
while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);
 
  if ( ($words[$g2419] > $GR2419LIM) || ($words[$g3719] > $GR3719LIM) || ($words[$g3724] > $GR3724LIM) ) {
    print $line;
    $filtered += 1;
  }

}
print " filtered out ",$filtered," points\n";
