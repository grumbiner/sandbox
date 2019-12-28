//rotate geographically-based grid points on to the cofs native grid
//Assumes that the u and v winds are on the same grid, and in
//geographic coordinates.

#include <stdio.h>
#include "ncepgrids.h"
#include "cofs.h"

void grid_derotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in, 
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) ;
void grid_rotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in, 
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) ;

int main(int argc, char *argv[]) {
  cfslevel<float> x; // a standard cofs level, including the info about
                     // grid angles.
  lambert<float> uvel_in, vvel_in; // vector components on geographic lambert
  lambert<float> mask;             //  grid
  cfsgrid<float> uvel, vvel; // lambert winds interpolated to cofs
  cfsgrid<float> uvel_out, vvel_out; // output winds on cofs native grid
  FILE *finu, *finv, *fout;
  float maskval = -9999., nonval = -99999.;

  finu = fopen(argv[1], "r");
  finv = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");
  if (finu == (FILE *) NULL || finv == (FILE *) NULL || 
      fout == (FILE *) NULL ) {
    if (finu == (FILE *) NULL) {
      printf("Failed to open the input u-vel file\n");
    }
    if (finv == (FILE *) NULL) {
      printf("Failed to open the input v-vel file\n");
    }
    if (fout == (FILE *) NULL) {
      printf("Failed to open the output file\n");
    }
    return 1;
  }

  mask.set(0.);
  do {
    uvel_in.binin(finu);
    vvel_in.binin(finv); 
    x.from(uvel_in, mask, maskval, nonval, uvel);
    x.from(vvel_in, mask, maskval, nonval, vvel);
    if ( !feof(finu) && !feof(finv) ) {
      grid_rotate(uvel, vvel, uvel_out, vvel_out, x);
      uvel_out.binout(fout);
      vvel_out.binout(fout);
      printf("rms u v -- eta %f %f\n",uvel_in.rms(), vvel_in.rms() );
      printf("rms u v -- cofs1 %f %f\n",uvel.rms(), vvel.rms() );
      printf("rms u v -- cofs2 %f %f\n",uvel_out.rms(), vvel_out.rms() );
    }
  } while ( !feof(finu) && !feof(finv) );

  fclose(fout);

  return 0;
}

  

void grid_derotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in, 
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) {
  int i;
  cfsgrid<float> angle;
  float pi = M_PI, rad = M_PI/180.;
  float dlon, dlat, dlnt, utmp, vtmp;
  ijpt loc, locp;

  printf("Entered grid_derotate\n"); fflush(stdout);
// Set up the rotation matrix
  for (loc.j = 0; loc.j < angle.ypoints() ; loc.j++) {
     locp.j = loc.j;
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++) {
     locp.i = loc.i + 1;
     if ( ! x.alon.in(locp) || ! x.alon.in(loc) ||
          ! x.alat.in(locp) || ! x.alat.in(loc)  ) {
        printf("point not legal on lat/long grid %3d %3d\n",loc.i, loc.j);
     }
     dlon = (x.alon[locp] - x.alon[loc])*cos(x.alat[loc]*rad);
     dlat =  x.alat[locp] - x.alat[loc];
     dlnt = sqrt(dlon*dlon + dlat*dlat);
     angle[loc] = asin(dlat/dlnt);
  }
  angle[locp] = angle[loc];
  }

//  Rotate back to the geographic grid from the COFS native grid
  for (loc.j = 0; loc.j < angle.ypoints() - 1; loc.j++ ) {
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++ ) {
     uvel_out[loc] = uvel_in[loc]*cos(angle[loc]) - 
                     vvel_in[loc]*sin(angle[loc]);
     vvel_out[loc] = uvel_in[loc]*sin(angle[loc]) + 
                     vvel_in[loc]*cos(angle[loc]);
  }
  }

  return;
}

void grid_rotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in, 
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) {
  int i;
  cfsgrid<float> angle;
  float pi = M_PI, rad = M_PI/180.;
  float dlon, dlat, dlnt, utmp, vtmp;
  ijpt loc, locp;

  printf("Entered grid_rotate\n"); fflush(stdout);
// Set up the rotation matrix
  for (loc.j = 0; loc.j < angle.ypoints() ; loc.j++) {
     locp.j = loc.j;
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++) {
     locp.i = loc.i + 1;
     if ( ! x.alon.in(locp) || ! x.alon.in(loc) ||
          ! x.alat.in(locp) || ! x.alat.in(loc)  ) {
        printf("point not legal on lat/long grid %3d %3d\n",loc.i, loc.j);
     }
     dlon = (x.alon[locp] - x.alon[loc])*cos(x.alat[loc]*rad);
     dlat =  x.alat[locp] - x.alat[loc];
     dlnt = sqrt(dlon*dlon + dlat*dlat);
     angle[loc] = asin(dlat/dlnt);
  }
  angle[locp] = angle[loc];
  }

// Now rotate from the geographic grid to the cofs native grid
  for (loc.j = 0; loc.j < angle.ypoints() - 1; loc.j++ ) {
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++ ) {
     uvel_out[loc] =   uvel_in[loc]*cos(angle[loc]) + 
                       vvel_in[loc]*sin(angle[loc]);
     vvel_out[loc] = - uvel_in[loc]*sin(angle[loc]) + 
                       vvel_in[loc]*cos(angle[loc]);
  }
  }

  return;
}
