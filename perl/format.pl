#!/usr/bin/perl
#
use POSIX;

#Quick and dirty perl to produce a draft WIGOS file for pilot project
#Initial version 19 February 2015 Robert Grumbine
#
$pdy = $ARGV[0];
$cycle = $ARGV[1];
open infile , "<$ARGV[2]";
open outfile, ">$ARGV[3]";

#Argument list is: PDY (yyyymmdd of input file), cycle (XXZ), name of input file, name of output file

#in cycle, Z is superfluous, so delete:
$cycle = substr($cycle,0,2);

#variable id should be read in.  For now it is a constant:
$varid = 110;

#Print out the invariant header:
print outfile "# TYPE=ADPSFC\n";
print outfile "#An_date=",$pdy,"\n";
print outfile "#An_time=",$cycle,"\n";
print outfile "#An_range=[",$cycle-3," to ",$cycle+3,")\n";
print outfile "#Status flag: See http:// ... web table\n";
print outfile "#Station_id,yyyymmdd,HHMMSS,latitude,Longitude,StatusFlag\n";

#Now loop through input file extract items, and print them out in standard format:
$count = 0;
while ($line = <infile>) {
  #this ensures that we space through file to the point that there's a report starting
  if ($line =~ /MESSAGE TYPE ADPSFC/) {
    $sidline = <infile>; #station id assumed always to follow ADPSFC
    $longline = <infile>; # .. followed by longitude
    $latline  = <infile>; # .. latitude
    $dhourline = <infile>; # delta hours from cycle time
    $l1 = <infile>; # dummy for the 'code table' line
    # Now loop until we find the last PQM file, and the 'END OF SUBSET' line
    $l1 = <infile>;
    #print $l1; if ($l1 =~ /PQM/) { print "found a pqm\n"; }  # debugging
    $tmp = 0;
    $l2 = $l1;
    while ($l2 =~ /PQM/) {
      $l2 = <infile>;
      $tmp += 1;
      #print $tmp," ",$l2; #debugging
      if ($l2 =~ /PQM/) {
        $l1 = $l2;
      }
    }
    if (!($l2 =~ /END OF SUBSET/)) {
      print "some kind of problem\n";
    }
    else {
# Print out only every other record, due to duplication of mass and XXX usage
      if (($count % 2) == 0 ) {
        $sid    = &nameval($sidline);
        $lon    = &getval($longline);
        $lat    = &getval($latline);
        $dhour  = &getval($dhourline);
        $qccode = &getval($l1);
        $hhmmss = &fixhours($cycle, $dhour);
        printf(outfile "%s,%8d,%06d,%9.5f,%9.5f,%d,%s,%d\n",$sid,$pdy,$hhmmss,$lat,$lon,$qccode,"NCEP",$varid);
      }
    }

  }
  $count += 1;
}

#A weirdness of perl: The return value is the value of the last statement
sub nameval {
  $tmp = substr $_[0], 35, 5;
}
sub getval {
  @words = split(/\s+/,$_[0]);
  # debug print $words[2],"\n";
  $words[2];
}
sub fixhours {
  local $ref, $delta, $tmp;
  # debug print "hrs ",$_[0]," ", $_[1],"\n";
  $ref   = $_[0];
  $delta = $_[1];
  $hh = $tmp / 1;
  $tmp = ($ref + $delta)*3600; #convert decimal hours to seconds
  $hh  = POSIX::floor( ($tmp - ($tmp % 3600))/3600);
  $tmp = $tmp % 3600;
  $mm  = ($tmp - $tmp % 60) / 60;
  $ss  = $tmp % 60;
  $hh = &zeroes($hh);
  $mm = &zeroes($mm);
  $ss = &zeroes($ss);
  $tmp = $hh . $mm . $ss;  # concatenation operator
  # debug print "hrs = ",$ref, " ", $delta, " ",$hh," ",$mm," ",$ss," ",$tmp,"\n";
  $tmp;
}
sub zeroes {
  $tmp = $_[0];
  if ($tmp >= 10) {
    $tmp;
  }
  else {
    $tmp = "0" . $tmp;
  } 
}
