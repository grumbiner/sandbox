#include "ncepgrids.h"

int main(void) {
  global_12th<unsigned char> mask, mask_old;
  global_12th<float> depths;
  global_12th<unsigned char> flags;
  FILE *fin;
  ijpt loc;
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
  palette<unsigned char> newer(19);

  newer.set_color(0,255,0,0); // error = red
  newer.set_color(1,128,128,128); // agreed boundary = gray
  newer.set_color(5,0,0,0); // agreed land = black
  newer.set_color(17,255,255,255); // agreed water = white
  newer.set_color(15,  0,  0,255); // new water = blue

  flags.set(22);
  fin = fopen("seaice_gland5min","r");
  if (fin != (FILE *) NULL) {
    mask_old.binin(fin);
    fclose(fin);
  }
  else {
    printf("failed to open mask file\n");
    return 1;
  }

  fin = fopen("bathyout","r");
  if (fin != (FILE *) NULL) {
    depths.binin(fin);
    mask.binin(fin);
    fclose(fin);
  }
  else {
    printf("failed to open mask file\n");
    return 1;
  }

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
  //unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
    if (mask_old[loc] == 195) mask_old[loc] = boundary;
    if (mask_old[loc] ==   0) mask_old[loc] = ocean;
    if (mask_old[loc] == 157) mask_old[loc] = land;
    if (mask_old[loc] >= 100) {
      printf("odd value in old %d %d  %d\n",loc.i, loc.j, mask_old[loc]);
      mask_old[loc] = land;
    }

// Both agree it is boundary:
    if (mask_old[loc] == boundary && mask[loc] == boundary) {
      flags[loc] = 1;
    }
// New has boundary, old has land agree it is boundary:
    if (mask_old[loc] == land && mask[loc] == boundary) {
      flags[loc] = 1;
    }
// Both agree it is water:
    if (mask_old[loc] == ocean && mask[loc] == ocean) {
      flags[loc] = water;
    }
    if (mask_old[loc] == water && mask[loc] == ocean) {
      flags[loc] = water;
    }
    if (mask_old[loc] == ocean && mask[loc] == water) {
      flags[loc] = water;
    }
    if (mask_old[loc] == water && mask[loc] == water) {
      flags[loc] = water;
    }

//Both agree it is land:
    if (mask_old[loc] == land && mask[loc] == land) {
      flags[loc] = land;
    }

// New water:
    if ( (mask_old[loc] == land || mask_old[loc] == boundary) &&
         (mask[loc] == water || mask[loc] == ocean) ) {
      flags[loc] = 15;
    }

    if (flags[loc] == 22) {
      printf("Some kind of error in flags %d %d  %d %d\n",loc.i, loc.j, mask_old[loc], mask[loc]);
      flags[loc] = 0;
    }

    
    //if (mask[loc] != land && mask[loc] != ocean) {
    //  printf("%d %d  %d\n",loc.i, loc.j, mask[loc]);
    //}
    if (mask[loc] != mask_old[loc] ) {
      printf("%4d %4d  %3d %3d\n",loc.i, loc.j, mask_old[loc], mask[loc]);
    }
  }
  }

// 224 195 157 100 0
  flags.xpm("flags.xpm",1,newer);

  palette<unsigned char> gg(19, 65);
  mask.xpm("mask.xpm",1,gg);

  return 0;
}
