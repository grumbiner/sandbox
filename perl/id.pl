#!/usr/bin/perl

while ($line = <STDIN>) {
  chop($line);
  @id = split(/\s+/,$line);
  #if ($id[$n] =~ "@") {
    $count{$id[1]} += 1;
  #
}
$sum = 0;
foreach $key (keys(%count)) {
  $sum += $count{$key};
  printf "%d visits by %s\n", $count{$key}, $key;
} 
print $sum," visits posts \n";
