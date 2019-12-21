#!/usr/bin/perl

#open(TIN,"items.sort") || die "failed to open items.sort";
open(TIN,"alpha") || die "failed to open items.sort";

while ($line = <TIN> ) {
  print $line;
  chop($line);
  $pri = <>;
  chop($pri);
  $rank{$line} = $pri; #puts a priority as the value for an assoc array, where
                       #the task is the key
  $pritypes{$pri} += 1;  #Track the types of priorities given
}

foreach $pri (keys(%pritypes) ) {
  print $pritypes{$pri}," ", $pri,"\n\n";
  foreach $j (keys(%rank) ) {
    if ($rank{$j} eq $pri) {
      print $pri," ",$j,"\n";
    }
  }
}

