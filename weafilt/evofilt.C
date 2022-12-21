#include "ssmi.h"
#include "ncepgrids.h"

// process concentration files towards developing good weather + ocean filter
// args are obs concentration file (floating point), land mask (uchar), ssmi file (ssmi_bufr_lines)
//

int main(int argc, char *argv[]) {
  FILE *fin;
  ssmi_bufr_line x;
  global_12th<float> conc;
  global_12th<unsigned char> land;
  int i, k, count = 0, notland = 0;
  latpt ll;
  ijpt loc;

  fin = fopen(argv[1], "r");
  conc.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  land.binin(fin);
  fclose(fin);

  fin = fopen(argv[3], "r");
  // 
  count = 0;
  while (!feof(fin)) {
    fread(&x, sizeof(ssmi_bufr_line), 1, fin);
    if (!feof(fin)) {
      for (i = 0; i < NSCANS; i++) {
        count++;
        ll.lat = x.full[i].latitude;
        ll.lon = x.full[i].longitude;
        loc = conc.locate(ll);
        if (land[loc] != 157) { notland++; 
        printf("%3d %3d  %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",(int) (conc[loc]*100+0.5), land[loc], ll.lat, ll.lon,
          x.full[i].t19v,
          x.full[i].t19h,
          x.full[i].t22v,
          x.full[i].t37v,
          x.full[i].t37h,
          x.full[i].t85v,
          x.full[i].t85h
        );
        }
               
        for (k = 0; k < 3; k++) {
          count++;
          ll.lat = x.full[i].hires[k].latitude;
          ll.lon = x.full[i].hires[k].longitude;
          loc = conc.locate(ll);
          if (land[loc] != 157) { notland++;
          printf("%3d %3d  %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",(int) (conc[loc]*100+0.5), land[loc], ll.lat, ll.lon,
            x.full[i].t19v,
            x.full[i].t19h,
            x.full[i].t22v,
            x.full[i].t37v,
            x.full[i].t37h,
            x.full[i].hires[k].t85v,
            x.full[i].hires[k].t85h
          );
          }
        }
      }
    }
  } // end of scanning through file

  printf("%d points are not land of the %d read\n",notland, count);

  return 0;
}
//void function(conc, land, latpt, 7xtb in, p(ice), p(land), p(water), p(coast) ) {
//
//return;
//}
