#include "ncepgrids.h"

// Analyze metfile contents 
int main(int argc, char *argv[]) {
  northgrid<float> tair, slp, rh, tsfc, swdn, lwdn, lwup, rprec, mask;
  northgrid<float> avger;
  palette<unsigned char> gg(19,65);

  FILE *fin;
  int i;
  ijpt sploc;
  latpt ll;

  ll.lat = 90.0;
  ll.lon =  0.0;
  sploc = tair.locate(ll);

  avger.set((float) 0.0);

  for (i = 1; i < argc; i++) {
    //printf("%s\n",argv[i]);
    fin = fopen (argv[i], "r");
    if (fin == (FILE *) NULL) {
      printf("Failed to open input file %s\n", argv[i]);
      return 1;
    }
    tair.ftnin(fin);
    slp.ftnin(fin);
    rh.ftnin(fin);
    tsfc.ftnin(fin);
    swdn.ftnin(fin);
    lwdn.ftnin(fin);
    //lwup.ftnin(fin);
    //rprec.ftnin(fin);
    //mask.ftnin(fin);
    fclose(fin);
  
    avger += lwdn;

  }
  avger /= (float) (argc - 1);
  printf("npole flux average = %f\n",avger[sploc] );

  avger.scale();
  avger.xpm("avg.xpm",7,gg);

  return 0;
} 
