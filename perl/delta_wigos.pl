#!/usr/bin/perl

# Compare two observation files developed for the WIGOS data quality monitoring demonstration project
# Arguments
#  Input: source file 1, source file 2
#  Output: destination file 

# Checks:
#   List all obs in file 1 but not in file 2
#   List all obs in file 2 but not in file 1
#   List all observations which differ in quality flag between the two sources
#   Tally number of observations which are in both files and which have same quality flag

# Input format requirements:
#   header lines (specific number)
#   unknown number of data lines (ID, YYYYMMDD, HHMMSS, latitude, longitude, status flag) 

# Robert Grumbine 2015

$header = 5; # Length of header

open fout, ">$ARGV[2]";

$id = 0;
$day = 1;
$time = 2;
$latitude = 3;
$longitude = 4;
$flag = 5;

$origina = &parse_header($ARGV[0], fina);
$originb = &parse_header($ARGV[1], finb);

print "origin of first file = ",$origina,"\n";
print "origin of second file = ",$originb,"\n";

#A bit of debugging here, to see that the right things are happening so far:
#$line = <fina>;
#print "first line in a is ",$line;
#
#$line = <finb>;
#print "first line in b is ",$line;


# Get the information from source 1:
$i = 0;
$dups = 0;
%hasha = ();
while ($line = <fina>) {
  chop($line);
  @words=split(/,/,$line);
  $base=$words[$id].",".$words[$day].",".$words[$time].",".$words[$latitude].",".$words[$longitude];
  #print $words[$id],"\n";
  #print $base,"\n";

  if ($hasha{$base} != 0) {
    print "duplicated a ",$base,"\n";
    $dups += 1;
  }
  else {
    $hasha{$base} = $words[$flag];
    $i += 1;
  }
}
print "Found ",$i," unique lines with data in file ",$origina,", with ",$dups," duplicates\n";

# Get the information from source 2:
$i = 0;
$dups = 0;
%hashb = ();
while ($line = <finb>) {
  chop($line);
  @words=split(/,/,$line);
  $base=$words[$id].",".$words[$day].",".$words[$time].",".$words[$latitude].",".$words[$longitude];

  if ($hashb{$base} != 0) {
    print "duplicated b ",$base,"\n";
    $dups += 1;
  }
  else {
    $hashb{$base} = $words[$flag];
    $i += 1;
  }
}
print "Found ",$i," unique lines with data in file ",$originb,", with ",$dups," duplicates\n";

#Now have all information in the two hash tables, hasha, hashb.  
#The key is all information other than the qc flag
#The value is the qc flag

#Walk through hash table a (source a) and print out any data that a has but b does not
$abutnotb=0;
$bbutnota=0;

#a bit of debug/demo:
#print $hasha{"fred"}," is hasha of fred -- a non-existent value\n";
#print $hasha{"02910,20150720,63.430000,21.070000"}," is hasha valua of an ob\n";
#$x = $hasha{"fred"};
#$y = $hasha{"02910,20150720,63.430000,21.070000"};

#if ($x == 0) {
#  print "a nonval = 0\n";
#}
#else {
#  print "nonval != 0\n";
#}

###################### Look for unique observations ##################################
#Work from source 1:
$count = 0;
while (  ($key, $value) = each(%hasha) ) {
  $count += 1;
  if ($hashb{$key} == 0) {
      # don't have ob in b
    print fout $origina," has observation,",$key,", but ",$originb," does not\n";
    $abutnotb += 1;
  }
}
print $origina," has ",$abutnotb," observations that ",$originb," does not\n";
print "a has ",$count," obs\n";

#Work from source 2:
$count = 0;
while (  ($key, $value) = each(%hashb) ) {
  $count += 1;
  if ($hasha{$key} == 0) {
      # don't have ob in a
    print fout $originb," has observation,",$key,", but ",$origina," does not\n";
    $bbutnota += 1;
  }
}
print $originb," has ",$bbutnota," observations that ",$origina," does not\n";
print "b has ",$count," obs\n";

################ Look for same/different qc flags on same observation ##########################
$same_flag = 0;
$diff_flag = 0;
while ( ($key, $value) = each(%hasha) ) {  #doesn't matter if we use a or b here, has to be in both
  if (($hasha{$key} == $hashb{$key}) && ($hashb{$key} != 0) ) {
    $same_flag += 1;
  }
  else {
    if ($hashb{$key} != 0) {
      $diff_flag += 1;
      print "differing qc,", $key, ",", $value,",",$hashb{$key},"\n";
    }
  }
}
print $same_flag," are the same qc flag ",$diff_flag," differ\n";
  






  


sub parse_header {
  local($filename, $input) = @_;
  local $origin, $i;

  unless (open $input, $filename) {
    print STDERR "Can't open $filename: $! \n";
    return;
  }

  $origin = 55;
  for ($i = 0; $i < $header; $i++) {
    $line = <$input>;

    if ($line =~ /ORIGIN=(\S+)/) {
      $origin=$1;
      #print "origin = ",$origin,"\n";
    }
  }
  if ($origin == 55 ) {
    $line = <$input>;
    @words = split(/,/,$line);
    $origin = $words[6];
    # kludge for keeping first line of data available after return
    close($input);
    open $input, $filename;
    for ($i = 0; $i < $header; $i++) {
      $line = <$input>;
    }
  }
 
  return $origin;
} 
