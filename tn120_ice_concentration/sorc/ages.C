#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_ice<unsigned char> ages;
  global_ice<float> agef;
  mvector<int> agesum(31);

  FILE *fin;
  int i;
  ijpt x;
  float area;
  
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL ) {
    printf("Failed to open the input file!\n");
    return 1;
  }

  ages.binin(fin);
  for (i = 0; i < agesum.xpoints(); i++) {
     agesum[i] = 0;
  }
  agef.set(1.0);
  area = agef.integrate();
  for (x.j = 0; x.j < ages.ypoints() ; x.j++) {
  for (x.i = 0; x.i < ages.xpoints() ; x.i++) {
     agesum[ (int) ages[x] ] += 1;
     agef[x] = ages[x];
  }
  }
  printf("Max, min ages %d %d\n", (int)ages.gridmax(), (int)ages.gridmin() );
    printf("Age distribution\n");
    for (i = 0; i <= ages.gridmax();  i++) {
       printf("%3d %6d\n",i,agesum[i]);
    }
  
  printf("Integral %f\n",(float) agef.integrate()/area );

  return 0;

}
