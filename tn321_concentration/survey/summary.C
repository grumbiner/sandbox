#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<unsigned char> tmp;
  global_12th<int> count, count224;
  FILE *fin;
  int i,j;
  latpt ll;
  ijpt loc;
  
  count.set(int(0));
  count224.set(int(0));

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    tmp.binin(fin);
    for (j = 0; j < tmp.xpoints()*tmp.ypoints() ; j++) {
       if (tmp[j] > 0 ) {
         if (tmp[j] <= 100) {
           count[j] += 1;
	 }
	 if (tmp[j] > 100) {
           count224[j] += 1;
	 }
       }
    }
    fclose(fin);

  }

  printf("count max min %d %d\n",count.gridmax(), count.gridmin() );
  printf("224   max min %d %d\n",count224.gridmax(), count224.gridmin() );
  for (loc.j = 0; loc.j < tmp.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < tmp.xpoints() ; loc.i++) {
    if (count[loc] > 0) {
      ll = tmp.locate(loc);
      printf("%f %f %d\n",ll.lon, ll.lat, count[loc]);
    }
  }
  }
  FILE *fout;
  fout = fopen("count.out","w");
  count.binout(fout);
  fclose(fout);

  palette<unsigned char> gg(19, 65);
  char fname[90];

  sprintf(fname,"count.xpm");
  printf("fname = %s\n",fname);
  count.xpm(fname, 41, gg); 

  return 0;
}
