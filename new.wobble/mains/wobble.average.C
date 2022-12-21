#include "ncepgrids.h"

// Reanalysis is T62

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  gaussian<float> press(62), tmp(62);
  int k, count = 0;
  palette<unsigned char> gg(19, 65);
  mvector<float> line(press.xpoints());

  printf("line.xpoints = %d\n",line.xpoints());
  press.set((float) 0.);
  
  for (int i = 0; i < argc; i++) {
    fin = fopen(argv[i],"r");
    if (fin == (FILE *) NULL) {
      return 1;
    }
    while ( !feof(fin) ) {
      k = tmp.binin(fin);
      if (k == tmp.xpoints()*tmp.ypoints()) {
        printf("%d max, min, average %f %f %f \n",count,  tmp.gridmax(), 
                 tmp.gridmin(), tmp.average() );
        count += 1;
        press += tmp;
      }
    }
    fclose(fin);
  }
  printf("count = %d\n",count);

  press /= count;
  fout = fopen("press.avg.bin","w");
  press.binout(fout);
  fclose(fout);
  printf("max, min, average %f %f %f \n", press.gridmax(), press.gridmin(), press.average() );
  press -= press.average();
  printf("max, min, average %f %f %f \n", press.gridmax(), press.gridmin(), press.average() );
  press.scale();
  press.xpm("average.xpm",1,gg);

  return 0;
 
}
