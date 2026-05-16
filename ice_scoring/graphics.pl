#!/usr/bin/perl
#Robert Grumbine
# 7 Dec 2012

#Universal:
print "'open icegraphics.ctl'\n";
print "'set gxout grfill'\n";
print "'clear'\n";
print "'set display color white'\n";
print "'set mpdset hires'\n";

$conclevels = 'set clevs 0 15 50 75 82.5 90 95 97.5 100';
$thicklevels= 'set clevs 0 0.05 0.25 0.50 0.625 0.75 0.875 1.0 1.125 1.25 1.5 1.75 2.0 ';
#
#--------------------------------------------------------
# mapping for southern hemisphere:
print "'set mproj sps'\n";
print "'set lat -90 -55'\n";
print "'set lon -240 120'\n";

# ----------------- Now begin the parts that we repeat umpty times for the time steps:
for ($i = 1; $i <= 8; $i++) {
  print "'set grads off'\n";
  print "'set strsiz 0.2'\n";
  print "'draw string 0.5 8.25 NOAA/NWS/NCEP/EMC Marine Modeling and Analysis Branch'\n";
  print "'draw string 3.5 7.95 Pre-Alpha CFS V2 model'\n";
  print "'draw string 2.5 7.65 Ice Concentration (0-100%) Day "; print $i; print "'\n";
  print "'set clevs 0 15 50 75 82.5 90 95 97.5 100'\n";
  print "'d conc";print 4*$i; print "*100'\n";
  print "'run cbarn'\n";
  print "'printim conc"; print $i; print "_aa.gif'\n";
  print "'clear'\n";
#
  print "'set grads off'\n";
  print "'set strsiz 0.2'\n";
  print "'draw string 0.5 8.25 NOAA/NWS/NCEP/EMC Marine Modeling and Analysis Branch'\n";
  print "'draw string 3.5 7.95 Pre-Alpha CFS V2 model'\n";
  print "'draw string 2.5 7.65 Ice Thickness (m) (0-100%) Day "; print $i; print "'\n";
  print "'set clevs 0 0.05 0.25 0.50 0.625 0.75 0.875 1.0 1.125 1.25 1.5 1.75 2.0 '\n";
  print "'d thick"; print 4*$i; print "'\n";
  print "'run cbarn'\n";
  print "'printim thick"; print $i; print "_aa.gif'\n";
  print "'clear'\n";
}
#

#--------------------------------------------------------
# mapping for northern hemisphere:
print "'set mproj nps'\n";
print "'set lat   50 90'\n";
print "'set lon -350 10'\n";
# ----------------- Now begin the parts that we repeat umpty times for the time steps:
for ($i = 1; $i <= 8; $i++) {
  print "'set grads off'\n";
  print "'set strsiz 0.2'\n";
  print "'draw string 0.5 8.25 NOAA/NWS/NCEP/EMC Marine Modeling and Analysis Branch'\n";
  print "'draw string 3.5 7.95 Pre-Alpha CFS V2 model'\n";
  print "'draw string 2.5 7.65 Ice Concentration (0-100%) Day "; print $i; print "'\n";
  print "'set clevs 0 15 50 75 82.5 90 95 97.5 100'\n";
  print "'d conc";print 4*$i; print "*100'\n";
  print "'run cbarn'\n";
  print "'printim conc"; print $i; print "_arctic.gif'\n";
  print "'clear'\n";
#
  print "'set grads off'\n";
  print "'set strsiz 0.2'\n";
  print "'draw string 0.5 8.25 NOAA/NWS/NCEP/EMC Marine Modeling and Analysis Branch'\n";
  print "'draw string 3.5 7.95 Pre-Alpha CFS V2 model'\n";
  print "'draw string 2.5 7.65 Ice Thickness (m) (0-100%) Day "; print $i; print "'\n";
  print "'set clevs 0 0.05 0.25 0.50 0.625 0.75 0.875 1.0 1.125 1.25 1.5 1.75 2.0 '\n";
  print "'d thick"; print 4*$i; print "'\n";
  print "'run cbarn'\n";
  print "'printim thick"; print $i; print "_arctic.gif'\n";
  print "'clear'\n";
}

#--------------------------------------------------------
# mapping for Alaska Region
print "'set mproj nps'\n";
print "'set lat  50  75'\n";
print "'set lon 165 230'\n";
# ----------------- Now begin the parts that we repeat umpty times for the time steps:
for ($i = 1; $i <= 8; $i++) {
  print "'set grads off'\n";
  print "'set strsiz 0.2'\n";
  print "'draw string 0.5 8.25 NOAA/NWS/NCEP/EMC Marine Modeling and Analysis Branch'\n";
  print "'draw string 3.5 7.95 Pre-Alpha CFS V2 model'\n";
  print "'draw string 2.5 7.65 Ice Concentration (0-100%) Day "; print $i; print "'\n";
  print "'set clevs 0 15 50 75 82.5 90 95 97.5 100'\n";
  print "'d conc";print 4*$i; print "*100'\n";
  print "'run cbarn'\n";
  print "'printim conc"; print $i; print "_alaska.gif'\n";
  print "'clear'\n";
#
  print "'set grads off'\n";
  print "'set strsiz 0.2'\n";
  print "'draw string 0.5 8.25 NOAA/NWS/NCEP/EMC Marine Modeling and Analysis Branch'\n";
  print "'draw string 3.5 7.95 Pre-Alpha CFS V2 model'\n";
  print "'draw string 2.5 7.65 Ice Thickness (m) (0-100%) Day "; print $i; print "'\n";
  print "'set clevs 0 0.05 0.25 0.50 0.625 0.75 0.875 1.0 1.125 1.25 1.5 1.75 2.0 '\n";
  print "'d thick"; print 4*$i; print "'\n";
  print "'run cbarn'\n";
  print "'printim thick"; print $i; print "_alaska.gif'\n";
  print "'clear'\n";
}

print "'quit'\n";
