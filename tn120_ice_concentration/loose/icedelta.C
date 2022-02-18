#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "ncepgrids.h"
#include "icessmi.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<DATTYPE> ref, newer;
  GRIDTYPE<float> delta, tmp;
  GRIDTYPE<unsigned char> land;
  FILE *fin, *fout;
  int cutoff;

  palette<unsigned char> gg(19,65);
  palette<unsigned char> hpal(21);
  int i;
  int total = 0, nodata = 0, weather = 0, baddata = 0;
  ijpt loc;
  int itmp = hpal.ncol / 2;

  fin = fopen(argv[1], "r");
  ref.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  newer.binin(fin);
  fclose(fin);
  fin = fopen(argv[3], "r");
  land.binin(fin);
  fclose(fin);
  if (ref.average() < 3.0) ref *= 100.;
  if (newer.average() < 3.0) newer *= 100.;
  cutoff = atoi(argv[4]);
  printf("cutoff = %d\n",cutoff);


  for (loc.j = 0; loc.j < newer.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < newer.xpoints(); loc.i++) {
    if (newer[loc] > (unsigned char) 100) { 
      newer[loc] = (unsigned char) 100 ;
    }
    if (ref[loc] > (unsigned char) 100) { 
      ref[loc] = (unsigned char) 100 ;
    }
  }
  }

  printf("new ice average, area %f %f\n",(float) newer.average(), newer.integrate()/1.e12 );

  printf("old ice average, area %f %f\n",(float)ref.average(), ref.integrate()/1.e12 );


  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
     delta[loc]  = (float) ref[loc];
     delta[loc] -= (float) newer[loc];
     if (newer[loc] == (unsigned char) NO_DATA && ref[loc] != (unsigned char) NO_DATA ||
         newer[loc] != (unsigned char) NO_DATA && ref[loc] == (unsigned char) NO_DATA) {
       nodata += 1;
       delta[loc] = 0;
     }
     if (newer[loc] == (unsigned char) WEATHER && ref[loc] != (unsigned char) WEATHER ||
         newer[loc] != (unsigned char) WEATHER && ref[loc] == (unsigned char) WEATHER) {
       weather += 1;
       delta[loc] = 0;
     }
     if (newer[loc] == (unsigned char) BAD_DATA && ref[loc] != (unsigned char) BAD_DATA ||
         newer[loc] != (unsigned char) BAD_DATA && ref[loc] == (unsigned char) BAD_DATA) {
       baddata += 1;
       delta[loc] = 0;
     }
     if (land[loc] > (unsigned int) 128) {
       delta[loc] = 0;
     }
     if (fabs(delta[loc]) >= cutoff) {
       printf("%3d %3d  %3d %3d %5.0f\n",loc.i, loc.j, (int) ref[loc], 
                           (int) newer[loc], delta[loc]);
       total += 1;
     } 
  }
  }
  printf("delta max min avg rms = %f %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average(), delta.rms() );
  printf("total number of changes %6d\n",total);
  printf("      number of no-data %6d\n",nodata);
  printf("      number of weather %6d\n",weather);
  printf("      number of baddata %6d\n",baddata);

// Set up delta palette and write out xpm:
  for (i = 0; i < hpal.ncol; i++) {
    unsigned char back = 0;
    float prop;
    if (i < itmp) { // less ice today than yesterday - reds
      prop = (itmp - i) ;
      prop /= (float) itmp;
      prop = pow((double)prop, (double)1./5.); //sqrt(sqrt(prop));
      prop *= 255.;
      //hpal.set_color(i, 255 - (unsigned char) prop, back, back);
      hpal.set_color(i, (unsigned char) prop, back, back );
    }
    else if ( i == itmp ) {
      hpal.set_color(i, back, back, back);
    }
    else {
      // more ice, blue
      prop = (i - itmp );
      prop /= (float) itmp;
      prop = pow((double)prop, (double)1./5.); //sqrt(sqrt(prop));
      prop *= 255.;
      //hpal.set_color(i, back, back, 255 - (unsigned char) prop );
      hpal.set_color(i, back , back, (unsigned char) prop );
    }
  }

// Compute change in area:
  for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
    if (newer[loc] > 128. || ref[loc] > 128. ) {
      tmp[loc] = 0.;
    }
    else if ( land[loc] > (unsigned char) 128. ) {
      tmp[loc] = 0.;
    }
    else {
      tmp[loc] = delta[loc];
    }
  }
  }
  printf("change in area   %f\n",tmp.integrate() / 1.e9 / 100.);

  for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
    if (tmp[loc] > 0.) {
      if (newer[loc] >= 15. && ref[loc] < 15.) { 
        tmp[loc] = 1.; 
      }
      else {
        tmp[loc] = 0.;
      }
    }
    else if (tmp[loc] < 0.) {
      if (newer[loc] <  15. && ref[loc] >= 15.) { 
        tmp[loc] = -1.;
      }
      else {
        tmp[loc] = 0.;
      }
    }
  }
  }
  printf("change in extent %f\n",tmp.integrate() / 1.e9 );


// Now rescale delta into the range of the color palette:
  delta /= (255./itmp);  
  delta += itmp;
//  printf("delta max min avg = %f %f %f\n",delta.gridmax(), 
//      delta.gridmin(), delta.average() );
  delta.xpm("delta.xpm",1,hpal);

  return 0;
}
