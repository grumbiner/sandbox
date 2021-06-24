#!/usr/bin/perl

#Perl to process the correlation file, on assumption that this is
# both faster and easier to write

$count=0;
while ($line = <STDIN> ) {
  chop($line) ;
  @words = split(/\s+/,$line);
  $lag = $words[4];
  $cor = $words[6];
  $count{$lag} += 1;
  $cor2{$lag} += $cor*$cor;

}
foreach $key (keys(%count)) {
  if ($key != 0) {
    printf "lag %3d has %3d instances, avg cor2 %f\n",
              $key, $count{$key}, $cor2{$key}/$count{$key};
  }
}
