#include "ncepgrids.h"
int main(int argc, char *argv[]) {
  int counts[12] = { 744, 672, 744, 720, 744, 720, 744, 744, 720, 744, 720, 744 };
  gaussian<double> total_sum(382), total_sum2(382);
  gaussian<double> sum(382), sum2(382);
  int i, allcount = 0;
  FILE *fin, *fout;
  
  total_sum.set(0.0);
  total_sum2.set(0.0);
  
  for (i = 0; i < 12; i++) {

    fin = fopen(argv[i+1], "r");
    sum.binin(fin);
    sum2.binin(fin);
    fclose(fin);

    printf("%s month %d max min %f %f\n",argv[i+1],i,sum.gridmax()/counts[i], sum.gridmin()/counts[i]);

    total_sum  += sum;
    total_sum2 += sum2;
    allcount += counts[i];

  }

  total_sum  /= allcount;
  total_sum2 /= allcount;
  gaussian<float> tmp(382), tmp2(382);
  printf("average max min %f %f\n",total_sum.gridmax(), total_sum.gridmin());
  printf("rms max min %f %f\n",sqrt(total_sum2.gridmax()), sqrt(total_sum2.gridmin()) );

  fout = fopen(argv[13], "w");
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
