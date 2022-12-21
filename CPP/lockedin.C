#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<float> sstin;
  global_12th<double> avg, sx2, var;
  global_12th<unsigned char> land;
  FILE *fin;
  int i, index, count = 0;
  ijpt loc;
  latpt ll;
  float toler = 0.01*1e4; 

  fin = fopen(argv[1],"r");
  land.binin(fin);
  fclose(fin);

  avg.set( 0.0);
  sx2.set( 0.0);
  
  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i],"r");
    sstin.binin(fin);
    fclose(fin);
    count += 1;
    if (sstin.gridmax() > 200) sstin -= 273.15;
    for (index = 0; index < sx2.xpoints()*sx2.ypoints(); index++) {
      avg[index] += sstin[index];
      sx2[index] += sstin[index]*sstin[index];
    }
  }

  //printf("found %d sst files\n",count);
  avg /= count;
  for (loc.j = 0; loc.j < sx2.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sx2.xpoints(); loc.i++) {
    var[loc] = (sx2[loc] - count * avg[loc]*avg[loc]) / count;
    if (var[loc] <= toler && land[loc] != 157 && land[loc] != 195 
                          && avg[loc] > -1.0 ) {
      ll = var.locate(loc);
      printf("%4d %4d  %7.2f %7.2f  avg %5.2f var %6.4f land %3d\n",loc.i, loc.j, 
           ll.lat, ll.lon, avg[loc], var[loc], land[loc]);
    }
    else if (land[loc] == 157 || land[loc] == 195) {
      var[loc] = 0.0;
      avg[loc] = 0.0;
    }
  }
  }

  fin = fopen("varout","w");
  //var.binout(fin);
  //fclose(fin);

  palette<unsigned char> gg(19, 65);
  for (loc.j = 0; loc.j < sx2.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sx2.xpoints(); loc.i++) {
    sstin[loc] = var[loc];
    //var[loc] = log(1.e-4 + var[loc]);
  }
  }
  //var.scale();
  //var.xpm("var.xpm",7,gg);

  sstin.binout(fin);
  fclose(fin);


  return 0;
}
