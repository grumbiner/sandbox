#!/usr/bin/perl

#Parse the navy gulf stream file in to a usable set of lat-long points.
#  Output is degrees north, degrees west.
#  No check is done for points which are identical or which are excessively
#  close.  Points are, however, required to be within 65 W to 81 W.
#Robert Grumbine 22 October 2002

$fout_north=$ARGV[0];
open fout_north, ">$ARGV[0]";
$fout_south=$ARGV[1];
open fout_south, ">$ARGV[1]";

while ($line = <STDIN>) {
  #VERBOSE print $line;
#Read through until hitting north wall line:
  $done = 0;
  if ($line =~ /NORTH WALL/ ) {
    #print "NORTH\n";
    #Loop through, creating north wall file, until reaching south wall
    $line = <STDIN>;
    while ($done != 1 ) {
     # print " line ",$line;
    if (!($line =~ /SOUTH WALL/ || $line =~ /EDDY/) ) {
      @words = split(" ",$line);
      #VERBOSE print "words ",$#words,"\n";
      for ($i = 0; $i <= $#words; $i++) {
        #VERBOSE2 print $i,"\n";
        if ($words[$i] =~ /(\d{2}\.\d{1})N(\d{2}\.\d{1})W/) {
          print $1," ",$2,"\n";
          print fout_north $1," ",$2,"\n";
        }
      }
    } #end of north wall if
    else {
      #VERBOSE print "found south wall or eddy \n";
      #VERBOSE print $line;
      $done = 1;
    }
    $line = <STDIN>;
    } #end of north wall while
  }

  if ($done == 1) {
    $done = 0;
   # print "SOUTH\n";
  #Loop through, creating south wall file, until reaching south wall
    $line = <STDIN> ;
#Test on $line Added 26 September 2006
    while ($done == 0 && $line ) {
      if ( !($line =~ /EDDY/) ) {
        @words = split(" ",$line);
        for ($i = 0; $i <= $#words; $i++) {
          if ($words[$i] =~ /(\d{2}\.\d{1})N(\d{2}\.\d{1})W/) {
            print $1," ",$2,"\n";
            print fout_south $1," ",$2,"\n";
          }
        }
      } #end of south wall if
      else {
        $done = 1;
      }
      $line = <STDIN>;
    } #end of south wall while
#Added 26 September 2006
    if (! $line) {
      print "Failed to find an eddy line in analysis\n";
    }

  }

}
