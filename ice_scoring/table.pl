#!/usr/bin/perl
#Robert Grumbine
# 8 Dec 2012

#Construct the core table for the sea ice model output
#3 models -- kiss, gfs, cfs
#3 regions -- alaska, aa, arctic
#2 parameters -- thickness, concentration
#8 time leads -- 1-8 days

@models=("kiss_v0.0.0", "gfs_v1.0.0", "cfs_v2.0.0");
@model_label=("KISS V0", "GFS V1", "CFS V2");

@regions=("alaska","aa","arctic");
@region_label=("Alaska","Antarctica","Arctic");

@params=("conc","thick");
@param_label=("Concentration","Thickness");

$maxlead=8;
$nregions=$#regions + 1;




for ($nr = 0; $nr < $nregions; $nr++) {
  print "<h3 align=\"center\">",$region_label[$nr],"</h3>\n";
  print "<table  align=\"center\"border=\"1\">\n";
  print "<tbody align=\"center\">\n";

#Table caption line:
  print "<tr><td>Lead</td>";
  for ($np = 0; $np <= $#params; $np++) {
    $parm = $param_label[$np];
    print "<td>",$parm,"</td>"
  }
  print "</tr>\n";

  #Now in a row for displays:
  for ($lead = 0; $lead < $maxlead; $lead++) {
    print "<tr><td>",$lead+1," day</td>";
    
    for ($np = 0; $np <= $#params; $np++) {
      print "<td>";
      $parm = $params[$np];
      for ($nm = 0; $nm <= $#models; $nm++) {
        print "<a href=\"",$models[$nm],"\/";
        print $parm,$lead+1,"_";
        print $regions[$nr],".gif\"\>";
        print $model_label[$nm]," </a>\n";
      }
      print "</td>\n";
    } #end parameter line

    print "</tr>\n";
  } #end lead
  
print "<\/tbody>\n";
print "<\/table>\n";

} #end regions
