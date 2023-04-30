#!/usr/bin/perl

while ($line = <stdin> ) {
  chop($line);
  if ($line =~ /..(\S+)/ ) {
    $l2 = $1;
  }
  $line = $l2;
  
  if ($line =~ "\.tar") {
    @names = split(/\./, $line);

    $dname = $names[0];
    for ($i = 1; $i <= $#names-1; $i++) {
      $dname = $dname . "." . $names[$i];
    }

    print "mkdir ",  $dname, "\n";
    print "cd ", $dname, "\n";
    print "tar xvf ../" . $dname . ".tar\n";
    print "cd ..\n";
    print "rm -f" . $dname . ".tar\n";
  }

}
