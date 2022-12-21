#include "mvector.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  time_series<float> glob(10104), nh(10104), sh(10104);
  int i;
  float gref, nref, sref;
  float g_gref, g_nref, g_sref;
  int ti;
  float tg, tn, ts;
  char s[4];

  fin = fopen(argv[1],"r");
  i = 0;
  while ( !feof(fin)) {
    fscanf(fin, "%f %f %f\n",&tg, &tn, &ts);
    glob[i] = tg;
    nh[i] = tn;
    sh[i] = ts;
    i++;
  }
  fclose(fin);

  gref = glob.autocovary(0);
  nref = nh.autocovary(0);
  sref = sh.autocovary(0);
  
  for (i = 0; i < 5*365+2; i++) {
    printf("%d %f %f %f \n",i, 
    //glob.autocovary(i)/gref/gref, 
    //nh.autocovary(i)/nref/nref, 
    glob.crosscorrel(glob, i),
    nh.crosscorrel(glob, i),
    sh.crosscorrel(glob, i));

  }

  return 0;
} 
