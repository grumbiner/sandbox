#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<float> te, rtg, dist, blend;
  global_12th<float> alpha;
  FILE *fin, *fout;
  //float length = 30; // km -- gaussian
  float length = 50; // km
  int i;

  fin = fopen(argv[1], "r");
  te.ftnin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  rtg.binin(fin);
  fclose(fin);
  fin = fopen(argv[3], "r");
  dist.binin(fin);
  fclose(fin);

  if (dist.gridmax() > 1e6) dist /= 1000;
  if (te.gridmax() < 200  ) te += 273.15;
  if (rtg.gridmax() < 200) rtg += 273.15;

  fout = fopen(argv[4], "w");

  for (i = 0 ;i < te.xpoints()*te.ypoints(); i++) {
//    alpha[i] = exp(-dist[i]*dist[i]/2./length/length); // alpha = weight for expectation/reference
    alpha[i] = exp(-dist[i]/length); // exponential doesn't (potentially) impose such high gradients nearshore 
  }
  
  blend  = rtg;
  te    -= rtg;
  te    *= alpha;
  blend += te;

  blend.ftnout(fout);
  te.ftnout(fout);
  fclose(fout);

  return 0;
}
