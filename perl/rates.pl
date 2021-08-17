#!/usr/bin/perl


while ($i = <STDIN> ) {
  @words = split(/ +/,$i);

  $byday{$words[0]} += 1;
  $bymon{$words[1]} += 1;
  $byname{$words[8]} += 1;
  $bysite{$words[6]} += 1; #Count number of times by site
  $sizeday{$words[0]} += $words[7];
  $sizesite{$words[6]} += $words[7];
  $sizefile{$words[8]} += $words[7];
  $rate = $words[7] / $words[5];
  print $words[5], " ", $words[7]," ", $rate," ", $words[6],"\n";

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
