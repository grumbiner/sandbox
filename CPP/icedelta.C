// Program to compute and display useful things in differencing ice
// fields, while remaining aware of all the ice flags and sanity values.
// Robert Grumbine 25 September 2003
//
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<DATTYPE> conc1, conc2, del, tmp;
  FILE *fin1, *fin2;
  ijpt loc;
  float flag = 0., power_scaling = 1.5;
  palette<unsigned char> hpal(21);
  int i, itmp = hpal.ncol / 2;
    

  fin1 = fopen(argv[1], "r");
  if (fin1 == (FILE *) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  fin2 = fopen(argv[2], "r");
  if (fin2 == (FILE *) NULL) {
    printf("failed to open %s\n",argv[2]);
    return 2;
  }
  
  conc1.binin(fin1);
  fclose(fin1);
  conc2.binin(fin2);
  fclose(fin2);
 
  power_scaling = atof(argv[4]);

// Ensure scaling is percents, rather than decimal
  if (conc1.average() < 3.) {
    conc1 *= 100.;
  }
  if (conc2.average() < 3.) {
    conc2 *= 100.;
  }

// Compute the delta field:
  del = conc2;
  for (loc.j = 0; loc.j < del.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < del.xpoints(); loc.i++) {
     if (del[loc] > 128 || conc1[loc] > 128) {
	del[loc] = flag;
     }
     else {
	del[loc] -= conc1[loc];
     }
  }
  }

// Set up delta palette and write out xpm:
  for (i = 0; i < hpal.ncol; i++) {
    unsigned char back = 0;
    float prop;
    if (i < itmp) { // less ice today than yesterday - reds
      prop = (itmp - i) ;
      prop /= (float) itmp;
      prop = pow(prop, 1./power_scaling); 
      prop *= 255.;
      //hpal.set_color(i, 255 - (unsigned char) prop, back, back);
      hpal.set_color(i, (unsigned char) prop, back, back );
    }
    else if ( i == itmp ) {
      hpal.set_color(i, 255, 255, 255);
    }
    else {
      // more ice, blue
      prop = (i - itmp );
      prop /= (float) itmp;
      prop = pow(prop, 1./power_scaling);
      prop *= 255.;
      hpal.set_color(i, back , back, (unsigned char) prop );
    }
  }

// Now rescale delta into the range of the color palette:
  tmp = del;
  tmp /= (255./itmp);
  tmp += itmp;
  tmp.xpm(argv[3],1,hpal);


// Now work on integral quantities:
  del /= 100.;
  printf("Change in area   = %6.0f km^2\n",del.integrate()/1.e6);

  for (loc.j = 0; loc.j < del.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < del.xpoints(); loc.i++) {
     if (conc1[loc] < 128 && conc2[loc] < 128) {
       if (conc1[loc] > 0 && conc2[loc] == 0. ) {
         del[loc] = -1.;
       }
       else if (conc1[loc] == 0 && conc2[loc] > 0.) {
         del[loc] = +1.;
       }
       else {
         del[loc] = 0;
       }
     }
     else {
       del[loc] = 0.;
     }
  }
  }
  printf("Change in extent = %6.0f km^2\n",del.integrate()/1.e6);

  return 0;
}
