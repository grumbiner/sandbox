#include <stdio.h>

#include "surface_tags.h"
#include "nesdis.h"
#include "ncepgrids.h"

template <class T>
T locmax(metricgrid<T> &x, ijpt &loc) ;
template <class T>
void border(metricgrid<T> &x, grid2<T> &y) ;

void filter(metricgrid<unsigned char> &gfilt, metricgrid<int> &gland, metricgrid<int> &gsea, metricgrid<float> &gpct, metricgrid<unsigned char> &oldland,
char *outname) { 

  grid2<float> tloc(gpct.xpoints(), gpct.ypoints() );
  float required_fraction = 0.80; //Fraction of zone required to be land
                                  // before declaring it land; conversely
                                  // less than 1-this is required to be ocean
                                  // before being declared ocean.

  long int absolute_land = 0, absolute_sea = 0;
  long int count = 0;
  int tmp;
  ijpt x;
  FILE *fout;
  palette<unsigned char> gg(19, 65);

  printf("Passed fixing the land, sea grids \n"); fflush(stdout);
    for (x.j = 0; x.j < gfilt.ypoints() ; x.j++) {
    for (x.i = 0; x.i < gfilt.xpoints() ; x.i++) {
      if (gland[x] >= 32 && gsea[x] == 0 ) {
        gfilt[x] = MASK;
        absolute_land += 1;
      }
      else if (gland[x] == 0 && gsea[x] >= 32) {
        gfilt[x] = NESSEA;
        absolute_sea += 1;
      }
      else {
        count += 1;
        tmp = gland[x] + gsea[x] ;
        if (tmp == 0) {
          gfilt[x] = UNKNOWN;
          continue;
        }
        if ( (float) gland[x] / tmp > required_fraction ) {
          gfilt[x] = MASK;
        }
        else if ( (float) gland[x] / tmp > (1.-required_fraction)  ) {
          gfilt[x] = COAST;
        }
        else {
          gfilt[x] = NESSEA;
        }
      }
      // Start figuring up fractional coverages:
      tmp = gland[x] + gsea[x] ;
      if (tmp != 0) {
        gpct[x] = (float) gland[x] / (float) tmp * 100.;
        #ifdef VERBOSE
          if (gpct[x] != 0. && gpct[x] != 100.) {
            printf("pct %6.3f  %3d %3d\n",gpct[x], x.i, x.j);
          }
        #endif
      }
// Note that the 157 and 195 are hardcoded here because in old versions of
//  the file, the land and coast masks were definitely those values, but
//  in future versions they may not be so.
      if (oldland[x] == 157) oldland[x] = MASK;
      if (oldland[x] == 195) oldland[x] = COAST;

      if ( (gfilt[x] == MASK  && oldland[x] != MASK) ||
           (gfilt[x] == NESSEA   && oldland[x] != 0   ) ||
           (gfilt[x] == COAST && oldland[x] != COAST) ) {

        printf("%3d %3d  %3d %3d  %4d %4d\n",x.i, x.j, 
             (int) gfilt[x], (int) oldland[x],
             gland[x], gsea[x] );
      }
    }
    }


// Done figuring.

    printf("Absolute land points %d\n",(int)absolute_land);
    printf("Absolute sea  points %d\n",(int)absolute_sea );
    printf("Decided points       %d\n",(int)count);

    gfilt.xpm("filt.xpm",14,gg);
    fout = fopen(outname, "w");
    gfilt.binout(fout);
    fclose(fout);

   // RG: changes for bordered (sidelobes) 
    border(gpct, tloc);
    tloc.xpm("pct.xpm",7, gg);
    printf("Integrated pctage: %f\n", (float) gpct.integrate()/100./1.e12 );



    for (x.j = 0; x.j < gfilt.ypoints() ; x.j++) {
    for (x.i = 0; x.i < gfilt.xpoints() ; x.i++) {
      if (gfilt[x] == (unsigned char) MASK ) {
        gpct[x] = 1;
      }
      else {
        gpct[x] = 0;
      }

      gland[x] = (int) gfilt[x] - (int) oldland[x]
                         + 128;
    }
    }
    gland -= gland.gridmin() ;
    printf("Delta max, min: %d %d\n",gland.gridmax(), gland.gridmin() );
    tmp = 2+(gland.gridmax() - gland.gridmin() ) / 19;
    gland.xpm("delta.xpm",tmp,gg);
    printf("Land in filt %f\n", (float) gpct.integrate()/1.e12 );

    return;
}

template <class T>
T locmax(metricgrid<T> &x, ijpt &loc) {
  T lmax;
  int i, j;
  ijpt tloc;
  lmax = x[loc];

  for (j = 0; j <=2; j++) {
  tloc.j = loc.j - 1 + j;
  for (i = 0; i <=2; i++) {
    tloc.i = loc.i - 1 + i;
    lmax = max(lmax, x[tloc]);
  }
  }  
 
  return lmax;
}
   
template <class T>
void border(metricgrid<T> &x, grid2<T> &y) {
  ijpt loc;
 
  y.set((T) 0);
  for (loc.j = 1; loc.j < x.ypoints()-2; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints()-2; loc.i++) {
     y[loc] = locmax(x, loc);
  }
  }
  return;
}
