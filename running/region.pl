#!/usr/bin/perl

$bot = $ARGV[0]/100;
$top = $ARGV[1]/100;

for ($i = 0; $i <= 13; $i++) {
  $sum[$i] = 0;
  $sumsq[$i] = 0;
}
$count += 1;

while ($line = <STDIN> ) {
  chop($line);
  @words=split(/\s+/, $line);
  $n = $#words;

  if ($n >= 12) {   
    # Construct statistics
    if ($words[3] > $bot && $words[3] < $top) {
      for ($i = 0; $i <= $n; $i++) {
         $sum[$i] += $words[$i];
         $sumsq[$i] += $words[$i]*$words[$i];
      }
      $tmp = $words[$n]*10;
      $cat = $tmp - ($tmp % 100);
      $cat /= 100;
      $histo{ $cat } += 1;
      $count += 1;
    }
  }
}

$count -= 1;
print $count," runners\n";
print "Mean leg ratios\n";
for ($j = 1; $j < 5; $j++) {
  printf(" %5.3f %6.1f %5.3f",$sum[1+($j-1)*3]/$count, $sum[2+($j-1)*3]/$count,  $sum[3+($j-1)*3] /$count );
}
printf (" %5.1f\n",$sum[$n]/$count);

print "\nS. Deviations:\n";
for ($i = 1; $i < $n-1; $i += 3) {
  printf(" %5.3f ",sqrt( ($sumsq[$i]-$sum[$i]*$sum[$i]/$count)/($count - 1) ) ) ;
  printf(" %5.2f",sqrt( ($sumsq[$i+1]-$sum[$i+1]*$sum[$i+1]/$count)/($count - 1) ) ) ;
  printf(" %5.3f",sqrt( ($sumsq[$i+2]-$sum[$i+2]*$sum[$i+2]/$count)/($count - 1) ) ) ;
}
printf(" %5.1f\n",sqrt( ($sumsq[$n]-$sum[$n]*$sum[$n]/$count)/($count - 1) ) ) ;

#print "Pace bins\n";
#while ( ($key,$value) = each(%histo) ) {
#    printf "pace %f  %4d runners \n",$key*10, $value;
#}
