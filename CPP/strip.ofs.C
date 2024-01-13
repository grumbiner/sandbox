#include "grid_math.h"

#define NX 1200
#define NY 1684
void vectorize(grid2<float> *x, int nparms, float flag, 
               mvector<mvector<float> > &y) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  grid2<float> x[12];
  mvector<mvector<float> > y;
  float flag;
  int i;

  for (i = 0; i < 12; i++) {
    x[i].resize(NX, NY);
  }

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    x[i-1].binin(fin);
    fclose(fin);
    if (i == 1) flag = x[i-1].gridmax();
    printf("max, min = %f %f\n",x[i-1].gridmax(flag), x[i-1].gridmin() );
  }
  
  vectorize(x, argc-1, flag, y);
  for (i = 0; i < argc - 1; i++) {
    printf("parm %d average = %f rms %f\n",i, y[i].average(), y[i].rms());
  }

  return 0;
}
void vectorize(grid2<float> *x, int nparms, float flag, 
               mvector<mvector<float> > &y) {
  ijpt loc;
  mvector<float> obs[NX*NY];
  int i, j, count = 0;
  printf("flag = %f\n",flag);
  printf("nparms = %d\n",nparms);

  for (loc.j = 0; loc.j < x[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x[0].xpoints(); loc.i++) {
    if (x[0][loc] != flag) {
      for (i = 0; i < nparms; i++) {
        obs[count].resize(nparms);
        obs[count][i] = x[i][loc]; 
      }
      count += 1;
    }
  }
  }
  printf("%d observations\n", count);

  y.resize(nparms);
  for (i = 0; i < nparms; i++) {
    y[i].resize(count);
    for (j = 0; j < count; j++) {
      y[i][j] = obs[j][i];
    }
  }
  //for (i = 0; i < nparms; i++) {
  //  printf("parm %d average = %f\n",i, y[i].average());
  //}
  return;
}
