#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  gaussian<double> total_sum(382), total_sum2(382);
  gaussian<float> sum(382), sum2(382), tmp(382), tmp2(382);
  gaussian<double> t1(382), t2(382);
  int i, allcount = 0, leap = 0;
  FILE *fin, *fout;
  char fname[90];
  
  total_sum.set(0.0);
  total_sum2.set(0.0);
  
  for (i = 0; i < 30; i++) {
    sprintf(fname,"fout.%4d.bin",(i+1980));
    fin = fopen(fname,"r");
    sum.binin(fin);
    sum2.binin(fin);
    fclose(fin);
    conv(sum, t1);
    conv(sum2, t2);
    if (i%4 == 0) {
      t1 *= 366.0;
      t2 *= 366.0;
      leap++;
      allcount += 366;
    }
    else {
      t1 *= 365.0;
      t2 *= 365.0;
      allcount += 365;
    }

    total_sum  += t1;
    total_sum2 += t2;
    printf("year %4d max min %f %f\n",i+1980,sum.gridmax(), sum.gridmin() );

  }

  total_sum  /= allcount;
  total_sum2 /= allcount;
  printf("average max min %f %f\n",total_sum.gridmax(), total_sum.gridmin());
  printf("rms max min %f %f\n",sqrt(total_sum2.gridmax()), sqrt(total_sum2.gridmin()) );

  fout = fopen("fout", "w");
  conv(total_sum, tmp);
  tmp.binout(fout);
  conv(total_sum2, tmp);
  tmp.binout(fout);
  fclose(fout);

// Histogram statistics:
  mvector<double> histo(50000); 
  double asum, csum;
  global_quarter<float> x, x2;
  float nonval = -999., flagval = -999.;
  ijpt loc;
  
// histogram on temperatures
  
  conv(total_sum, tmp);
  x.set((float) 0.0);
  x.fromall(tmp, flagval, nonval);
  printf("x  max, min = %e %e\n",x.gridmax(), x.gridmin() ); fflush(stdout);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] < -100) {
      printf("failure on %4d %4d val %f\n",loc.i, loc.j, x[loc]);
    }
  }
  }
  histo = 0.0;
  printf("starting histogram on temperatures\n"); fflush(stdout);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    histo [ max(0,lrintf ((x[loc]+100)*10.)) ] += x.cellarea(loc);
  }
  }
  printf("passed main loop\n"); fflush(stdout);
  asum = 0.0; csum = 0.0;
  for (i = 0; i < histo.xpoints(); i++) {
    if (histo[i] != 0) {
      asum += histo[i];
      csum += histo[i]*(i/10.0-100);
      printf("%6.2f  %9.5f  %9.5f\n",i/10.0-100, histo[i]/1e12, asum/1e12);
      fflush(stdout);
    }
  }
  printf("Global mean temperature %f\n",csum/asum);
  fflush(stdout);
 
// histogram on variances
  conv(total_sum2, tmp2);
  x2.fromall(tmp2, flagval, nonval);
  printf("x2 max, min = %e %e\n",x2.gridmax(), x2.gridmin() ); fflush(stdout);
  histo = 0.0;
  double var;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    var = sqrt(max(0.0, x2[loc]-x[loc]*x[loc] ) );
    histo [ max(0,lrintf (10.*var)) ] += x2.cellarea(loc);
  }
  }
  asum = 0.0; csum = 0.0;
  for (i = 0; i < histo.xpoints(); i++) {
    if (histo[i] != 0) {
      asum += histo[i];
      csum += histo[i]*i/10.0;
      printf("%4.1f  %9.5f  %9.5f\n",(float)i/10.0, histo[i]/1e12, asum/1e12); 
    }
  }
  printf("Global mean sqrtvar %f\n",csum/asum);

  return 0;
}
