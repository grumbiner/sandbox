#!/usr/bin/perl

#@a = <STDIN>;
#print @a;

#while ( $i = <STDIN> ) {
#  #print $i;
#  if ($i =~ /field/ ) {
#    print "Found the character i\n";
#  }
#}


##\S means a character that is not a non-printing (blank, tab, ..)
#while ( $i = <STDIN> ) {
#  if ( $i =~  /\@\S/ ) {
#    print $i ;
#  }
#  else {
#    print "other ", $i ;
#  }
#}

while ($i = <STDIN> ) {
  @words = split(/ +/,$i);

  $byday{$words[0]} += 1;
  $bymon{$words[1]} += 1;
  $byname{$words[8]} += 1;
  $bysite{$words[6]} += 1; #Count number of times by site
  $sizesite{$words[6]} += $words[7];

  @address = split(/\./,$words[6]);
  $n=$#address;
#  print $address[$n-1],"\n";
  $domain1{$address[$n]} += 1;
  $domain1_bytes{$address[$n]} += $words[7];
  $tmp[0] = $address[$n - 2];
  $tmp[1] = $address[$n - 1];
  $tmp[2] = $address[$n];
  $tmp2 = join(".", @tmp) ;
  $domain2{$tmp2} += 1;
  $domain2_bytes{$tmp2} += $words[7];
}

print "testing assoc array to sbr ",&testassoc(%byday),"\n";

#-------------Print out statistics---------------
print "\nDay of week visitation\n";
while ( ($key,$value) = each(%byday) ) {
  print $key," had ",$value," accesses\n";
}

print "\nMonthly visitation\n";
while ( ($key,$value) = each(%bymon) ) {
  print $key," made ",$value," accesses\n";
}

#Do a pseudo-sort by frequency of retrieval
$top = &findmax( values(%byname) );
$bottom = $top / 2; 
while ( $top > 1 ) {
  while ( ($key,$value) = each(%byname) ) {
  if ($value > $bottom && $value <= $top) {
    printf "%5d retrievals of %s\n",$value, $key;
  }
  }
  $top = $bottom;
  $bottom = $top / 2;
}

print "\nSite visitation (greater than 100 files)\n";
while ( ($key,$value) = each(%bysite) ) {
  if ($value > 100) {
    print $value," acceses by ",$key," \n";
  }
}

print "\nSite byte retrievals > 10 Mb\n";
while ( ($key,$value) = each(%sizesite) ) {
  if ($value > 10*1024*1024) {
    printf "%7.1f Mb retrieved by %s\n", $value/1024/1024,$key; 
  }
}

print "\nDomain stats \n";
while ( ($key, $value) = each(%domain1) ) {
  if ($value > 1 && $domain1_bytes{$key}/1024/1024 > 1.) {
    printf "%6d accesses for %7.1f Mb by %s\n", $value, 
             $domain1_bytes{$key}/1024/1024,$key;
  }
}

print "\nLevel 2 Domain stats \n";
while ( ($key, $value) = each(%domain2) ) {
  if ($value > 1 && $domain2_bytes{$key}/1024/1024 > 1.) {
    printf "%6d accesses for %7.1f Mb by %s\n", $value, 
             $domain2_bytes{$key}/1024/1024,$key;
  }
}

sub findmax {
  local $tmp1;
  $tmp1 = 0;
  foreach $i (@_) {
    if ($i > $tmp1) {
      $tmp1 = $i;
    }
  }
  $tmp1;
}
sub testassoc {
  local $tmp1;
  print "size of associative array passed in is ",$#_, "\n";
  foreach $i (@_) {
    print $i,"\n";
  }
  $tmp1 = $#_; 
}
