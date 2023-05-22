#include <stdio.h>

#include "ncepgrids.h"
#define TOLER 0.1

int main(void) {
  northgrid<unsigned char> nin, nland;
  psgrid<unsigned char> *nout, *noutland;
  FILE *fin, *finland;
  float rescale = .25;
  float line_space = 5;
  ijpt x, y;
  fijpt fx;
  latpt loc;
  palette<unsigned char> gg(19,65);

// Get original data
  fin = fopen("north","r");
  finland = fopen("nland.map","r");
  nin.binin(fin);
  nland.binin(finland);
  fclose(fin);
  fclose(finland);

// Define the new grids in terms of the old:
  nout = new psgrid<unsigned char> (
            nin.xpoints() / rescale, 
            nin.ypoints() / rescale, 
           -nin.xorig/(nin.dx * rescale), 
           -nin.yorig/(nin.dy * rescale), 
            nin.slat, nin.slon, nin.sgn, 
            nin.dx*rescale, nin.dy*rescale);
  noutland = new psgrid<unsigned char> (nin.xpoints() / rescale, 
            nin.ypoints() / rescale, -nin.xorig/(nin.dx * rescale), 
           -nin.yorig/(nin.dy*rescale), nin.slat, nin.slon, nin.sgn, 
            nin.dx*rescale, nin.dy*rescale);
 
// Now loop over latitudes/longitudes and zero nout if on the integral pts,
//    else, copy over from corresponding nin pt.
  for (x.j = 0; x.j < nout->ypoints(); x.j++) {
  for (x.i = 0; x.i < nout->xpoints(); x.i++) {
    y.i = (int) (0.5 + ( (float)x.i * rescale));
    y.j = (int) (0.5 + ( (float)x.j * rescale));
    fx.i = (float) x.i * rescale;
    fx.j = (float) x.j * rescale;
    loc = nin.locate(fx);
    if ( fabs(loc.lat/line_space - (int) (loc.lat/line_space + 0.5) ) < TOLER/line_space ) {
      //printf("close lat at %d %d  %f %f\n",x.i, x.j, loc.lat, loc.lon);
      nout->grid[x.i+x.j*nout->xpoints() ] = 0;
    }
    else if ( fabs( fabs(loc.lon/line_space) - (int) (fabs(loc.lon/line_space) + 0.5) ) < TOLER/line_space*2. ) { 
      //CDprintf("close lon at %d %d  %f %f  %f %f\n",x.i, x.j, loc.lat, loc.lon, fx.i, fx.j);
      //printf("close lon at %d %d  %f %f  %f %f\n",x.i, x.j, loc.lat, loc.lon);
      nout->grid[x.i+x.j*nout->xpoints() ] = 0;
    }
    else {
      nout->grid[x.i+x.j*nout->xpoints() ] = nin[y];
    }
  }
  }

  nout->xpm("nout.xpm",15,gg);
  nin.xpm("nin.xpm",15,gg);

  return 0;
}
