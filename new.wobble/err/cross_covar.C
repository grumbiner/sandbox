#include "mvector.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  time_series<float> glob(10106), nh(10106), sh(10106);
  time_series<float> g_glob(10106), g_nh(10106), g_sh(10106);
  int i;
  float gref, nref, sref;
  float g_gref, g_nref, g_sref;
  int ti;
  float tg, tn, ts;
  char s[4];

  fin = fopen(argv[1],"r");
  i = 0;
  while ( !feof(fin)) {
    fscanf(fin, "%d %s %f %f %f\n",&ti, &s, &tg, &tn, &ts);
    //printf("%d %f %f %f\n",ti, tg, tn, ts);
    glob[i] = tg;
    nh[i] = tn;
    sh[i] = ts;

    i++;
  }
  fclose(fin);

  fin = fopen(argv[2],"r");
  i = 0;
  while ( !feof(fin)) {
    fscanf(fin, "%d %s %f %f %f\n",&ti, &s, &tg, &tn, &ts);
    g_glob[i] = tg;
    g_nh[i] = tn;
    g_sh[i] = ts;

    i++;
  }
  fclose(fin);

  gref = glob.autocovary(0);
  nref = nh.autocovary(0);
  sref = sh.autocovary(0);
  
  g_gref = g_glob.autocovary(0);
  g_nref = g_nh.autocovary(0);
  g_sref = g_sh.autocovary(0);
  for (i = 0; i < 5*365+2; i++) {
    printf("%d %f %f %f  ",i, 
    glob.autocovary(i)/gref, 
    nh.autocovary(i)/nref, 
    sh.autocovary(i)/sref    );
    printf(" %f %f %f  ",
    g_glob.autocovary(i)/g_gref, 
    g_nh.autocovary(i)/g_nref, 
    g_sh.autocovary(i)/g_sref    );

    printf(" %f %f %f\n",glob.crossvary(g_glob, i)/g_gref/gref,
                        nh.crossvary(g_nh, i)/g_nref/nref,
                        sh.crossvary(g_sh, i)/g_sref/sref      );
  }

  return 0;
} 
