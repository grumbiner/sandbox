#!/usr/bin/perl

$p1 = 0;
$m1 = 0;
$p2 = 0;
$m2 = 0;
$del1 = 0.0;
$del2 = 0.0;
$rms1 = 0.0;
$rms2 = 0.0;
$count = 0;
$big1 = 0;
$big2 = 0;

while ($line = <STDIN>) {
  chop($line);
  @args = split(" ",$line);
  $ref = $args[3];
  $method1 = $args[5];
  $method2 = $args[7];
  if (abs( ($ref - $method1) / $ref) > 0.025 ) {
    $big1 += 1;
  }
  if (abs( ($ref - $method2) / $ref) > 0.025 ) {
    $big2 += 1;
    print $line," ",($ref - $method1)/$ref," ", ($ref - $method2)/$ref,"\n"
  }
  if ($ref > $method1) {
    $p1 += 1;
  }
  else {
    $m1 += 1;
  }
  if ($ref > $method2) {
    $p2 += 1;
  }
  else {
    $m2 += 1;
  }
  $del1 += ($ref - $method1)/$ref;
  $del2 += ($ref - $method2)/$ref;
  $rms1 += ($ref - $method1)*($ref - $method1) / $ref / $ref;
  $rms2 += ($ref - $method2)*($ref - $method2) / $ref / $ref;
  $count += 1;
#  print $ref, $method1, $method2, "\n";
#  print $line,"\n";
}
print $del1," ", $del2, " ",$del1/$count," ", $del2/$count," ",$count,"\n";
print $rms1," ", $rms2, " ",$count,"\n";
print "method 1, ref > method ",$p1," < ",$m1," big differences ",$big1,"\n";
print "method 2, ref > method ",$p2," < ",$m2," big differences ",$big2,"\n";
