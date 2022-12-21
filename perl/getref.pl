#!/usr/bin/perl
#Robert Grumbine 2 June 2000

#Get the referring sites
while ($line = <STDIN>) {
  if ( ($site) = ($line =~ /exist: (\S+),/ ) ) {
    $missing{$site} += 1;
  }
  if ( ($site) = ($line =~ /referer: (\S+)/ ) ) {
    $referrers{$site} += 1;
  }
}

while ( ($key, $value) = each(%referrers) ) {
  print $value,' ',$key,"\n";
}

while ( ($key, $value) = each(%missing) ) {
  print 'missing ',$value,' ',$key,"\n";
}


