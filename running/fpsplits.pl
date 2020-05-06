#!/usr/bin/perl

$final_dist = (26.2 - 18.0);
$length[0] = 6.3;
$length[1] = 5.8;
$length[2] = 5.9;
$length[3] = (26.2 - 18.0);

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
    #print $words[$n-3],"\n";
    if ($n > 22) {   
      $splits[3] = $words[$n-3];
      $splits[2] = $words[$n-7]; 
      $splits[1] = $words[$n-10]; 
      $splits[0] = $words[$n-14]; 
      #print $splits[0]," ",$splits[1]," ",$splits[2]," ",$splits[3]," \n";
      for ($i = 0; $i < 4; $i++) {
         $splits[$i] = &tosecs($splits[$i]);
      #   printf(" %f ",$splits[$i] );
      }
      if ( $splits[3] != 0 ) {
        $fsplit = $splits[3] / 26.2;
        printf(" %5.3f ", $splits[0]/$splits[3]) ;
        printf(" %5.1f ", $splits[0]/$length[0]) ;
        $pace[0] =  ($splits[0]/$length[0])/$fsplit;
        $pace[1] =  (($splits[1]-$splits[0])/$length[1])/$fsplit;
        $pace[2] =  (($splits[2]-$splits[1])/$length[2])/$fsplit;
        $pace[3] =  (($splits[3]-$splits[2])/$length[3])/$fsplit;
        printf(" %5.3f ", $pace[0]);
        for ($i = 1; $i < 4; $i++) {
          printf(" %5.3f ", ($splits[$i]-$splits[$i-1])/$splits[3]) ;
          printf(" %5.1f ", ($splits[$i]-$splits[$i-1])/$length[$i]) ;
          printf(" %5.3f ", $pace[$i]);  #pace ratio
        }
        printf(" %5.1f ",$fsplit ); #Final average

        # Construct statistics
        if ($splits[0] == 0 || 
            $splits[1] == 0 ||
            $splits[2] == 0 ||
            $splits[3] == 0 ||
            $fsplit == 0            )  {
          print $line," delete ";
        }
        else {
          for ($i = 0; $i <= 3; $i++) {
             $sum[$i] += $pace[$i];
             $sumsq[$i] += $pace[$i]*$pace[$i];
          }
          $count += 1;
        }
        print "\n";
      }
    }
  }

  else {
    print " no ",$line,"\n";
  }
   
}

$count -= 1;
print $count," runners\n";
print "Mean leg ratios\n";
for ($i = 0; $i < 5; $i++) {
    printf(" %5.3f ",$sum[$i]/$count);
}
print "\n";

print "\nVariances:\n";
for ($i = 0; $i < 5; $i++) {
    printf(" %5.3f ",sqrt( ($sumsq[$i]-$sum[$i]*$sum[$i]/$count)/($count - 1) ) ) ;
}
print "\n";


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
