#!/usr/bin/perl
#getaddrinfo(dsl-201-128-5-129.prod-infinitum.com.mx, AF_INET)

while ($line = <STDIN> ) {
  if ($line =~ /getaddrinfo/  ) {
    #print $line;
    if ($line =~ /getaddrinfo\((\S+)\,/  ) {
      $count{$1} += 1;
    }
  }
}

while ( ($key,$value) = each(%count) ) {
  print $value," count for ",$key,"\n";
}

