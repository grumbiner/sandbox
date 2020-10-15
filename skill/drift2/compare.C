#include "grid_math.h"

#define SKILES 207
#define NDAYS   16
#define MAXBUOYS 50000
#define nmtokm 1.852

#include <cstdio>
#include <cstring>

void getfcst(FILE *fin, float *dir, float *dist, int &code) ;
extern "C" void fit_(float *odist, float *dist, int &n, float &b0, float &b1,
                     float &correl);

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2;
  int i, j, k, retcode;

  float dir1[NDAYS][SKILES], dist1[NDAYS][SKILES];
  float dir2[NDAYS][SKILES], dist2[NDAYS][SKILES];
  float t1[NDAYS*SKILES], t2[NDAYS*SKILES];
  float b0, b1, correl;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
 
  getfcst(fin1, &dir1[0][0], &dist1[0][0], retcode);
  getfcst(fin2, &dir2[0][0], &dist2[0][0], retcode);

  k = 0;
  for (i = 0; i < NDAYS; i++) {
  for (j = 0; j < SKILES; j++) {
     printf("%5.1f %5.1f  %5.1f %5.1f\n",dir1[i][j], dir2[i][j], dist1[i][j], dist2[i][j]);
     t1[k] = dist1[i][j];
     t2[k] = dist2[i][j];
     k++;
  }
  }

  k = NDAYS*SKILES;
  fit_(t1, t2, k, b0, b1, correl);
  printf("b0 b1 %f %f, correl = %f\n",b0, b1, correl);

  return 0;
}
#define nskile 207
#define ndays   16

/* C++ language variant finally, 2005/03/07 */
void getfcst(FILE *fin, float *dir, float *dist, int &code) {

  int i, j, skpt2;
  float lat, longit, t1, t2;
  
  char header[900], trailer[900];

  for (i = 0; i < ndays; i++) {
    fgets(header,800,fin);
    //printf("%s\n",header);
    fgets(header,800,fin);
    //printf("%s\n",header);
    fgets(header,800,fin);
    //printf("%s\n",header);
    fgets(header,800,fin);
    //printf("%s\n",header);
    fgets(header,800,fin);
    //printf("%s\n",header);

    for (j = 0; j < nskile; j++) {
      fscanf(fin,"%d %f %f",&skpt2, &dir[j+nskile*i], &dist[j+nskile*i]);
      //printf("i j skpt dir dist  %d %d %d %f %f\n",i,j,skpt2,dir[j+nskile*i],dist[j+nskile*i]);
      //fflush(stdout);
    }
    fgets(header,800, fin);
    fgets(header,800, fin);
    //printf("header, %s\n",header);
    j = 0;
    do {
      fgets(header,800,fin);
      sscanf(header,"%d %f %f %f %f",&skpt2, &longit, &lat, &t1, &t2);
      //printf("strlen %d  %d %f %f %f %f\n",strlen(header), skpt2, longit, lat, t1, t2);
      j += 1;
    }  
    while (!feof(fin) && strlen(header) > 28); 
 
    fscanf(fin, "%s",trailer);
  }

  code = ndays;

  return;
}
