#!/usr/bin/perl

$final_dist = (50.2 - 41.9) ;

for ($i = 0; $i < 7; $i++) {
  $sum[$i] = 0;
  $sumsq[$i] = 0;
}
$count += 1;

while ($line = <STDIN> ) {
  chop($line);
  @words=split(/\s+/, $line);
  #print $#words,"\n";
  $n = $#words;

  if ( $words[$n - 1] =~ /\d{1,}:\d{2}:\d{2}/ ) {
    #print $words[$n-1],"\n";
    if ($n > 12) {   
      $splits[5] = $words[$n];
      $splits[4] = $words[$n-2]; 
      $splits[3] = $words[$n-4]; 
      $splits[2] = $words[$n-6]; 
      $splits[1] = $words[$n-8]; 
      $splits[0] = $words[$n-10]; 
      for ($i = 0; $i < 6; $i++) {
         $splits[$i] = &tosecs($splits[$i]);
      }
      print $words[$n-1];
      for ($i = 0; $i < 5; $i++) {
         printf(" %5.3f ", $splits[$i]/$splits[5]) ;
      }
      $fsplit = &tosecs($words[$n-1]) - &tosecs($words[$n-3]);
      $fsplit /= $final_dist;
      printf(" %5.3f ",$fsplit / $splits[5]);
      if ($splits[0] == 0 || 
          $splits[1] == 0 ||
          $splits[2] == 0 ||
          $splits[3] == 0 ||
          $splits[4] == 0 ||
          $splits[5] == 0 ||
          $fsplit == 0            )  {
        print " delete ";
      }
      else {
        for ($i = 0; $i <= 5; $i++) {
           $sum[$i] += $splits[$i] / $splits[5];
           $sumsq[$i] += $splits[$i]*$splits[$i] / $splits[5]/$splits[5];
        }
        $sum[6] += $fsplit/$splits[5];
        $sumsq[6] += $fsplit*$fsplit/$splits[5]/$splits[5];
        $count += 1;
      }
      print "\n";
    }
  }

  #else {
  #  print " no ",$words[$n],"\n";
  #}
   
}

$count -= 1;
print $count," runners\n";
print "Mean leg ratios\n";
for ($i = 0; $i < 7; $i++) {
  if ($i != 5) {
    printf(" %5.3f ",$sum[$i]/$count);
  }
}
print "\n";

print "\nVariances:\n";
for ($i = 0; $i < 7; $i++) {
  if ($i != 5) {
    printf(" %5.3f ",sqrt( ($sumsq[$i]-$sum[$i]*$sum[$i]/$count)/($count - 1) ) ) ;
  }
}


sub tosecs {
  local $tmp1;
  @times=split(/:/, $_[0]);
  if ($#times == 1) {
    $tmp1=$times[0]*60 + $times[1];
  }
  else {
    $tmp1=$times[0]*3600 + $times[1]*60 + $times[2]; 
  }
  $tmp1;
}
