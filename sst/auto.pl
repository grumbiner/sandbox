#!/usr/bin/perl

# make up .gs files for harmonic amplitudes -- group by 10s to 100.
print "\'open harmonics.ctl\'\n";
print "\'set gxout grfill\'\n";
print "\'clear\'\n";
print "\'enable print b.grads\'\n";
print "\'set grads off\'\n";
print "\'set display color white\'\n";
print "\'set mpdset hires\'\n";
print "\'set lon 30 390\'\n";
print "\'set strsiz 0.2\'\n";


for ($i = 1; $i <= 32; $i++) {
  print "#\n";
  print "\'set grads off\'\n";
  print "\'set strsiz 0.2\'\n";
  print "\'draw string 3.00 7.75 Harmonic ",$i," Amplitude\'\n";
  print "\'set clevs 0 0.01 0.05 0.10 0.2 0.3 0.4 0.5 0.75 1.0\'\n";
  print "\'d ampl",$i," / 100\'\n";
  print "\'run cbarn\'\n";
  print "\'printim ampl",$i,".gif\'\n";
  print "\'print\'\n";
  print "\'clear\'\n";
}
  print "\'quit\'\n";
for ($i = 1; $i <= 32; $i++) {
  print "ampl",$i," 0 254 255\n";
  print "phase",$i," 0 254 255\n";
}
