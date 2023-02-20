#!/usr/bin/perl

$x="/com/prod/stuff/nonsense";
@parts=split(/\//,$x);
for ($i = 1; $i < $#parts; $i++) {
  $build=$build."/".$parts[$i];
  print $parts[$i],"  ",$build,"\n";
  $y = system(" [ -d $build ] ");
  print "y = ", $y,"\n"
}

