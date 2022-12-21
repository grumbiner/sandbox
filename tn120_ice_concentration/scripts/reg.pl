#!/usr/bin/perl

#Read in a simplified table of region elements and write back out a 
#  table with parameters for polar stereographic grids, and a table
#  suitable for inclusion in a web page
#Robert Grumbine 8 December 2000

open htmlout, ">htmlout";

while ($line = <STDIN>) {
  chop($line);
  @words=split(",", $line);
#  for ($i = $#words; $i >= 1 ; $i--) {
#    print $words[$i],"\n";
#  }
  $region = $words[0];
  $lons = 0;
  $lats = 0;
  for ($i = 1; $i <= $#words; $i++) {
#    print $i," ",$words[$i],"\n";
    $words[$i] =~ /(\d+)\s+(\D+)/;
    $val = $1;
    $test = $2;
    if ($test =~ /w|e/i) {
      $lon[$lons ] = $val;
      if ($test =~ /w/i) {
        $lon[$lons ] *= -1.0;
      }
      $lons += 1;
    }
    if ($test =~ /n|s/i) {
      $lat[$lats] = $val;
      if ($test =~ /s/i) {
        $lat[$lats] *= -1.;
      }
      $lats += 1;
    }
  }
  #diagnostic output
  print $region,"\n";
  print $lon[0]," ",$lat[0],"\n";
  print $lon[1]," ",$lat[1],"\n";

  #construct html table entries:
  print htmlout "<TR>";
  printf htmlout "<TD>%s</TD> <TD>%d %d</TD> <TD>%d %d</TD>",$region, 
                  $lon[0], $lat[0], $lon[1], $lat[1];
  print htmlout "</TR>\n";

  #call a diagnostic program to get ij values of corners
  system "echo $region >> locations";
  system "./a.out $lon[0] $lat[0] $lon[1] $lat[1] >> locations";

}
