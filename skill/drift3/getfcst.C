#include <cstdio>
#include <cstring>
#define nskile 207
#define ndays   16

void getfcst(int &date, FILE *fin, float *dir, float *dist, int &code) ;

/* C++ language variant finally, 2005/03/07 */
void getfcst(int &date, FILE *fin, float *dir, float *dist, int &code) {

  int i, j, skpt2;
  float lat, longit, t1, t2;
  
  char header[900], trailer[900];

  for (i = 0; i < ndays; i++) {
    fgets(header,800,fin);
    fgets(header,800,fin);
    fgets(header,800,fin);
    fgets(header,800,fin);
    fgets(header,800,fin);

    for (j = 0; j < nskile; j++) {
      fscanf(fin,"%d %f %f",&skpt2, &dir[j+nskile*i], &dist[j+nskile*i]);
    }
    fgets(header,800, fin);
    fgets(header,800, fin);
    j = 0;
    do {
      fgets(header,800,fin);
      sscanf(header,"%d %f %f %f %f",&skpt2, &longit, &lat, &t1, &t2);
      j += 1;
    }  
    while (!feof(fin) && strlen(header) > 28); 
 
    fscanf(fin, "%s",trailer);
  }

  code = ndays;

  return;
}
