#include "ncepgrids.h"

#include "icessmi.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<ssmipt> grid;
  GRIDTYPE<unsigned char> flag, conc;
  ijpt loc;
  FILE *fin;
  palette<unsigned char> scale(3);
  int ratio = 2;

  scale.set_color(0, 0, 0, 0);   // black for equal
  scale.set_color(1, 255, 0, 0); // red for more weather
  scale.set_color(2, 0, 0, 255); // blue for more ice

  fin = fopen(argv[1],"r");
  grid.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < flag.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < flag.xpoints(); loc.i++) {
    if (grid[loc].obs.weather_count > grid[loc].obs.count/ratio) {
      flag[loc] = 1;
      if ( grid[loc].obs.hires_conc != WEATHER &&  grid[loc].obs.hires_conc != NO_DATA) {
        printf("%3d %3d  %2d %2d  %3d\n",loc.i, loc.j, grid[loc].obs.weather_count, 
                                  grid[loc].obs.count, grid[loc].obs.hires_conc);
      }
    }
    else if (grid[loc].obs.weather_count < grid[loc].obs.count/ratio) {
      flag[loc] = 2;
    }
    else {
      flag[loc] = 0;
      if ( grid[loc].obs.hires_conc != WEATHER &&  grid[loc].obs.hires_conc != NO_DATA) {
        printf("%3d %3d  %2d %2d  %3d\n",loc.i, loc.j, grid[loc].obs.weather_count, 
                                  grid[loc].obs.count, grid[loc].obs.hires_conc);
      }
    }

//  If good enough observations, use concentration:
    if (flag[loc] == 2) {
      conc[loc] = grid[loc].obs.hires_conc;
    }
    else if (grid[loc].obs.hires_conc == NO_DATA) {
      conc[loc] = NO_DATA;
    }
    else if (grid[loc].obs.hires_conc == BAD_DATA) {
      conc[loc] = BAD_DATA;
    }
    else {
      conc[loc] = WEATHER;
    }
  }
  }

  flag.xpm(argv[2],1,scale);
  fin = fopen("filtered","w");
  conc.binout(fin);
  fclose(fin);

  return 0;
}
