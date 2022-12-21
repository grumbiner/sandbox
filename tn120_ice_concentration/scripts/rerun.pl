#!/usr/bin/perl

#Version to rerun dates that did earlier produce data files, though
# with problems:

while ($line = <STDIN>) {
  chop($line);
  if ($line =~ /\d+/) {
    @tag=split(/\s+/,$line);
    $mon=$tag[0]; $mon *= 1.0; 
    $day = $tag[1]; $day *= 1.0;
    if ($mon < 10) {$mon = "0" . $mon;}
    if ($day < 10) {$day = "0" . $day;}
    $dtgroup = "2000" . $mon . $day;
    system "./piece.sh $dtgroup $region";
  }
  else { 
    #print "word line ",$line,"\n";
    $region = $line;
  }
}
