#include <stdlib.h>

#include "ncepgrids.h"

#if (RESOPS == 1)
  #include "resops.h"
#endif

// Program to read in initial estimate of mask and bathymetry, and
//   then pave over water ('water' or 'ocean') points which are
//   disconnected from a point specified at input.  Optionally, users
//   may specify a file name which contains a number of lat-long pairs
//   which are boundary locations for straits which it is desirable to
//   close.  
// Arguments are:
//   name of combined bathymetry and mask file
//     -- bathymetry is assumed to be a float
//     -- mask is assumed to be an unsigned character.  
//     -- to change these assumptions, modify the type declarations
//          for inbathy and inmask
//   latitude of the central (ocean) point
//   longitude of central (ocean) point.
//     -- negative is degrees west
//     -- it is important that the point be in the grid, on an ocean point.
//        beyond that, it doesn't matter where.
//   name of straits file -- optional
//     -- If there are some straits you want to pave over, 
// Robert Grumbine 19 August 2003
// Mod 9 Jul 2007: exclude resops.h unless we're working on resops grids

int bcount = 0;
int filled = 0;
#include "geometry.C"
// Downscale constructs a factor of 'scaling' smaller grid (larger dx, dy)
// from the input grid, preserving all 'boundary' flagged points at the
// expense of fill or undefined.
// Upscale does the reverse step.  Purpose of pair is to permit filling of
// large domain fields, as Eurasia, without overruning the ability of
// the system to permit recursive calls in boundary fill.
template <class T>
void downscale(llgrid<T> &tot_field, llgrid<T> &field, int scaling);
template <class T>
void upscale(grid2_base<T> &tot_field, grid2_base<T> &field, int scaling);

template <class T>
bool nearest(grid2_base<T> &field, ijpt &loc, T type) ;

void newFill(const int x, const int y, const unsigned char fill,
     const unsigned char boundary, grid2<unsigned char> &field) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout, *fstraits;
  TARGET<float> inbathy, outbathy;
  TARGET<unsigned char> inmask, outmask;
  TARGET<unsigned char> tmp_mask;
// note that only usage of tmp_mask is in downscale, which resets
//   grid definition anyhow.  Avoiding 'FAMILY' may bring some
//   additional clarity to the program.  10 Jul 2007

  latpt central;
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
  unsigned char paved = 13, final_ocean = 16;

  ijpt loc;
  fijpt floc;
  unsigned char to_fill;
  palette<unsigned char> gg(19,65);
  int x, y;
  char straitline[900];
  

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file %s\n",argv[1]);
    return 1;
  }
  inbathy.binin(fin);
  inmask.binin(fin);
  fclose(fin);

  central.lat = atof(argv[2]);
  central.lon = atof(argv[3]);
  printf("Location of chosen point is: %f lat %f lon \n",
            central.lat, central.lon);
  fflush(stdout); 

  outbathy = inbathy;
  outmask = inmask;
  floc = outbathy.locate(central);
  loc = floc;
  if ( outmask[loc] == water ) {
    to_fill = ocean;
  }
  else if ( outmask[loc] == ocean) {
    to_fill = water;
  }
  else {
    printf("the point you specified is not water or ocean %d\n",
        (int) outmask[loc] );
    return 1;
  }
  
// For paving purposes, flip all non-(water ocean) points to boundary
  for (loc.j = 0; loc.j < outbathy.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < outbathy.xpoints(); loc.i++) {
    if (outmask[loc] == land ||
        outmask[loc] == undef ||
        outmask[loc] == to_fill ) {
      outmask[loc] = boundary;
    }
  }
  }
  // Need also to fill in the peripheral points so as to let blockfill stay
  // inside region
  loc.j = 0;
  for (loc.i = 0; loc.i < outbathy.xpoints(); loc.i++) {
    outmask[loc] = boundary;
  }
  loc.j = outbathy.ypoints() - 1;
  for (loc.i = 0; loc.i < outbathy.xpoints(); loc.i++) {
    outmask[loc] = boundary;
  }
  loc.i = 0;
  for (loc.j = 0; loc.j < outbathy.ypoints(); loc.j++) {
    outmask[loc] = boundary;
  }
  loc.i = outbathy.xpoints() - 1;
  for (loc.j = 0; loc.j < outbathy.ypoints(); loc.j++) {
    outmask[loc] = boundary;
  }

// Pave over straits, if there are any:
  if (argc == 6) {
    fijpt fll, fur;
    latpt ll, ur;
    fstraits = fopen(argv[5],"r");
    if (fstraits == (FILE *) NULL) {
      printf("Failed to open the input file %s\n",argv[5]);
      return 1;
    }
    while (!feof(fstraits) ) {
      fgets(straitline, 90, fstraits);
      sscanf(straitline,"%f %f %f %f\n",&ll.lat, &ll.lon, &ur.lat, &ur.lon);
      printf("%f %f  %f %f\n",ll.lat, ll.lon, ur.lat, ur.lon); 
      fll = outbathy.locate(ll);
      fur = outbathy.locate(ur);
      lineFill(fll, fur, outmask, boundary);
    }
  }


// Do a fill on a smaller-scale version of the grid. Currently only llgrids
//   (implicitly -- have to read the code to tell)
  #if (RESOPS == 2)
  downscale(outmask, tmp_mask, 10);
  floc = tmp_mask.locate(central);
  loc  = floc;
  x = loc.i; y = loc.j;
  newFill(x, y, final_ocean, boundary, tmp_mask);
  upscale(outmask, tmp_mask, 10);
  #endif
  
// Now conduct the fill from the central point:
  floc = outbathy.locate(central);
  loc = floc;
  printf("initial point is at %d %d\n",loc.i, loc.j);
  x = loc.i; y = loc.j;
  newFill(x, y, final_ocean, boundary, outmask);

  // Loop through looking for points adjacent to final_ocean which are
  //  not boundary and fill those:
  // note that with newfill recursing on x then y, better results here
  //   with j being the inner loop.
  for (loc.i = 1; loc.i < outmask.xpoints()-1; loc.i++) {
  for (loc.j = 1; loc.j < outmask.ypoints()-1; loc.j++) {
    if (nearest(outmask, loc, final_ocean) && outmask[loc] != boundary &&
        outmask[loc] != final_ocean) {
      x = loc.i; y = loc.j;
      newFill(x, y, final_ocean, boundary, outmask);
    }
  }
  }

  
  fout = fopen(argv[4], "w");
  outbathy.binout(fout);
  outmask.binout(fout);
  fclose(fout);

  outmask.xpm("mask.xpm",1, gg);
  outbathy.scale();
  outbathy.xpm("bathy.xpm",8, gg);

  return 0;

}
template <class T>
bool nearest(grid2_base<T> &field, ijpt &loc, T type) {
  int index = loc.i + loc.j*field.xpoints();
  return (
    field[ index + 1] == type ||
    field[ index - 1] == type ||
    field[ index + field.xpoints()] == type ||
    field[ index - field.xpoints()] == type     ); 
}

////////////////////////////////////////////////////////////////
//Default case for subsampling rather than metric grids
template <class T>
void downscale(grid2_base<T> &tot_field, 
               grid2_base<T> &field,
               int scaling) {

  printf("Would have tried to do a downscale, exiting instead\n");
  exit(1);

}

template <class T>
void downscale(llgrid<T> &tot_field, llgrid<T> &field,
            int scaling) {
// Totfield is input, field is output
  int nx, ny;
  ijpt loc, delta, tloc;
  T boundary = 1, undef = 3;

  field.dlat = tot_field.dlat * scaling;
  field.dlon = tot_field.dlon * scaling;
  field.firstlat = tot_field.firstlat - tot_field.dlat/2. + field.dlat/2.;
  field.firstlon = tot_field.firstlon - tot_field.dlon/2. + field.dlon/2.;

  if (tot_field.xpoints() % scaling == 0 &&
      tot_field.ypoints() % scaling == 0   ) {
    field.resize(tot_field.xpoints() / scaling, tot_field.ypoints() / scaling);
  }
  else {
    // ensure that new grid has at least enough points:
    nx = (tot_field.xpoints() / scaling) + 1;
    ny = (tot_field.ypoints() / scaling) + 1;
    #ifdef VERBOSE
      printf("rescaling %d %d by %d to %d %d\n",tot_field.xpoints(),
                  tot_field.ypoints(), scaling, nx, ny);
    #endif
    field.resize(nx, ny);
  }

  // Should now have grid available.  Loop through source grid and 
  field.set(undef);
  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    //VERBOSEprintf("downscaling to %d %d\n",loc.i, loc.j); fflush(stdout);
    for (delta.j = 0; delta.j < scaling; delta.j++) {
    for (delta.i = 0; delta.i < scaling; delta.i++) {
      tloc = loc;
      tloc.i *= scaling;
      tloc.j *= scaling;
      tloc += delta;
      if (tot_field.in(tloc) ) {
        if (tot_field[tloc] == boundary) {
          field[loc] = boundary;
        }
      }
    }
    }
  }
  }
  
  return;

}
template <class T>
void upscale(grid2_base<T> &tot_field, 
             grid2_base<T> &field,
             int scaling) {
  ijpt tloc, loc, delta; 
  T boundary = 1, undef = 3;
  
  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    for (delta.j = 0; delta.j < scaling; delta.j++) {
    for (delta.i = 0; delta.i < scaling; delta.i++) {
      tloc = loc;
      tloc.i *= scaling;
      tloc.j *= scaling;
      tloc += delta;
      if (tot_field.in(tloc) ) {
        if (tot_field[tloc] != boundary && field[loc] != boundary && 
              field[loc] != undef) {
          tot_field[tloc] = field[loc];
        }
      }
    }
    }
  }
  }
  
  return ;
}
void newFill(const int x, const int y, const unsigned char fill,
                  const unsigned char boundary, grid2<unsigned char> &field) {
  bcount += 1;

  if ( x < 0 || x > field.xpoints() - 1 ||
       y < 0 || y > field.ypoints() - 1 ||
       bcount > BCOUNT_LIM   ) {
    bcount -= 1;
    return;
  }

  //if (filled % 5000 == 0) { printf("filled = %d\n",filled); }

  if ( (field[x+y* field.xpoints()] != boundary) &&
       (field[x+y* field.xpoints()] != fill)        ) {
    field[x+y* field.xpoints()] = fill;
    filled += 1;

    newFill(x+1, y, fill, boundary, field);
    newFill(x, y+1, fill, boundary, field);
    newFill(x-1, y, fill, boundary, field);
    newFill(x, y-1, fill, boundary, field);
  }

  bcount -= 1;
  return;

}
