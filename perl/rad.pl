#!/usr/bin/perl

$cp = 1004;         # specific heat
$i0 = 1367/4.;      # top of atmosphere downwelling radiation
$h = 10e3;          # scale height
$zstar = 1000e3;     # 'top of atmosphere'
$rhonot = 1.2;      # surface density
$alpha  = 0.3;      # albedo
$a      = 1.45e-4;  # m^2 / kg, 1.45e-4 leads to isothermal lower atm.
$c1 = $a*$rhonot*$h;
#Note that I0 has been set to 1.
 
#for ($i = 100; $i >= 0; $i -= 0.5) {
for ($i = 0; $i < 100; $i += 0.5) {
  $rho = $rhonot * exp(-$i*1000/$h);
  $idown = exp(-$c1*(exp(-$i*1000/$h) -exp(-$zstar/$h) ) );
  $iup   = $alpha * exp(-$c1*(2. - exp(-$zstar/$h) - exp(-$i*1000/$h) ) ); 
  print $i, " ", $idown," ",$rho*$idown," ",$iup, " ",$rho*$iup," ",
        $rho*($idown + $iup)," ",$a/$cp *($idown + $iup)*86400.*$i0, "\n";
}
#Surface effects:
$q0 = (1. - $alpha)*exp(-$c1*(1.-exp(-$zstar/$h))) ;
$ts = $q0 *$i0*86400. / 2.8e3 / 2.e3 / .15;
print "surface, ",$ts,"\n";

