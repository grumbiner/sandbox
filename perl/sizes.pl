#!/usr/bin/perl

#Usage:
#  webstst.pl sitestats filestats single_visit < log file
#
#Process web log files (hopefully much, much faster than
#  getstats in the case of large files
#form of log entry: 
# requestor dummy dummy [date:time +zone] "type file protocol" code size
#    0        1    2     3           4     5    6      7        8    9
#Drops error'ed files on the fly.
#Don't count the usage by the branch either
#Robert Grumbine
#4 January 2000

$freq=2000;
$count = 0;

$siteout = $ARGV[0];
open siteout, ">$ARGV[0]";

$fileout = $ARGV[1];
open fileout, ">$ARGV[1]";

$only = $ARGV[2];
open only, ">$ARGV[2]";


while ($line = <STDIN>) {
  $count += 1;
  chop($line);
  @words = split(/\s+/, $line); #split on the spaces
  $codes{$words[8]} += 1;
#Don't process if there was an error, or it is a branch request
  if ( ($words[8] != 404 && $words[8] != 400) && !( $words[0] =~ /wwb.noaa.gov/  ) ) {
    $from{$words[0]} += 1;
    $reqfrom{$words[0]} = $words[6]; 
    $dummy1{$words[1]} += 1;
    $dummy2{$words[2]} += 1;
    $file{$words[6]} += 1;
    $proto{$words[7]} += 1;
    $totalbytes += $words[9] / 1024 / 1024 ;
    $bysize{$words[9]} += 1;

    @date = split(/\//, $words[3]);
    $month{$date[1]} += 1; 
  
    if ( &typecheck($words[5])) { $type{$words[5]} += 1; }
    else {
      print STDERR $ok, " invalid type ",$words[5],"\n";
      print STDERR $line,"\n";
    }

    if ( ($count % $freq) == 0) {
      print STDERR "#";
    }
    if ( ( $count % ($freq * 25) ) == 0) {
      print STDERR "\n";
    }
  } #End of 'if code is ok'
  else {
    #can get here either by bad code or by wwb visitation
    if ( $words[8] == 404 || $words[8] == 400 ) {
      $errfile{$words[6]} += 1;
    }
  }
}

print "Total files accessed ",$count,"\n";
print "Total Mbytes accessed ",$totalbytes,"\n";

print fileout "\nFiles by size and number of requests\n";
while ( ($key, $value) = each(%bysize) ) {
  printf fileout ("%d  %d   %d bytes %f Mb\n",$key, $value, $key*$value, $key*$value/1024/1024.);
}

print fileout "\nFiles requested\n";
while ( ($key, $value) = each(%file) ) {
    printf fileout ("%5d %s\n",$value, $key);
}


sub typecheck {
  @checklist = ("\"GET", "\"HEAD", "\"POST");
  $ret=0;
  foreach $i (@checklist) {
    #print $i," is var tested vs. ",$_[0],"\n";
    if ( $_[0] == $i) { $ret = 1; return $ret; }
  }
  return $ret;
}
  
