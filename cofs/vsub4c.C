#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h"
#include "cofs.h"

#define NX 360
#define NY 151
#define NZ  34

// Program to conduct the vertical interpolation between Chalikov Ocean 
//   Model and Coastal Forecast System model.
void vgrid(mvector<float> &z) ;

int main(void) {

  cfslevel<float> cfsmetric;

// Begin vertical interpolation block
  ijpt x, y;
  cfsgrid3<float> t3(NZ);
  cfsgrid3<float> t3out(19);
  cfsgrid<float> tmp;
  mvector<float> sound(NZ), vert(19);
  metricvector<float> cfschal(NZ), cfsfin(19);
  FILE *fout, *fin;
  float flag = -99.9;
  int i, j;
  mvector<float> chalz(NZ);
  grid2<float> transects(t3out.xpoints(), t3out.nz);
  grid2<float> transecte(t3out.ypoints(), t3out.nz);
  char fname[80];

  fout = fopen("bndyout", "w");
  fin = fopen("cfsout", "r");
  if (fout == NULL) { 
    printf("Could not open bndyout file\n");
    return -1;
  }
  if (fin == NULL ) {
    printf("Could not open cfsout input file\n");
    return -1;
  }
  
// Begin a section to look at doing the vertical interpolation
// Establish the cfschal metric here -- the chalikov vertical
//   depth scale, on the cfs horizontal grid.
//   leads to cfschal.scale(tempor);
   vgrid(chalz);
   //chalz.printer(stdout);
   cfschal.set_metric(chalz);
   //cfsmetric.z.printer(stdout);

// Read in the t3 data.
   for (j = 0; j < NZ; j++) {
      tmp.read(fin);
      t3.put_layer(j, tmp);
//      tmp.read(fin); // salt
//      tmp.read(fin); // u
//      tmp.read(fin); // v
   }

// For all points on grid (perimeter):
   for (j = 0; j < cfsmetric.alat.ypoints(); j++) {
     printf("j = %d\n", j); fflush(stdout);
     x.j = j;
     for (i = 0; i < cfsmetric.alat.xpoints(); i++) {
       x.i = i; 
       printf("At point %d %d ", x.i, x.j); fflush(stdout);
       printf(" depth = %f\n", cfsmetric.depth[x] ); fflush(stdout);
    
      vert = cfsmetric.z * (float) ( -1.* cfsmetric.depth[x] );
      cfsfin.set_metric(vert);

      t3.get_sounding(x, cfschal);
      cfsfin.interp(cfschal, flag);
      cfsfin.binout(fout); //need to write the vector write out function,
                        // also need to find the format of the output needed.
//CD      cfsfin.printer(stdout);
      t3out.put_sounding(x, cfsfin);  //not necessary at this point, but
                                      // put the new sounding into the 3d array.
  
      }
    }
    fclose(fout);

   fout = fopen("transect", "w");
   if (fout == NULL ) { 
     printf("failed to open transect output file\n");
     return -1;
   }
   x.i = 0;
   x.j = 0;
   y.i = t3out.xpoints()-1;
   y.j = 0;
   t3out.get_transect(x, y, transects);
   //transects.printer(stdout);

   x.i = t3out.xpoints()-1;
   x.j = t3out.ypoints()-1;
   t3out.get_transect(y, x, transecte);
   //transecte.printer(stdout);

   for (i = 0; i < 12; i++) {
    // print out the file 12 times for CFS peculiar read in method
     transects.binout(fout);
     transecte.binout(fout);
   }

   fclose(fout);

  return 0;
  
}

// Generate the Chalikov vertical discretization
void vgrid(mvector<float> &z) {
  float power = 3.0, z0 = 100.0, dz0 = 5.0 , depthm = 5000.;
  int l = 34;
  mvector<float> zi(35);
  mvector<float> xi(35);
  mvector<float> dz(35);
  float xih, dxih;

  int i;

  xih = pow( log(((depthm - l*dz0)+z0)/z0), (1./power) ); 
  dxih = xih / l;

  for (i = 1; i <= l; i++) {
     xi[i] = dxih* i;
  }
  for (i = 1; i <= l; i++) {
     zi[i] = z0*(exp( pow(xi[i],power) ) - 1.0);
  }

  dz[1] = zi[1] + dz0;
  for (i = 2; i <= l; i++) {
     dz[i] = zi[i] - zi[i-1] + dz0;
  }

  zi[0] = 0.;
  zi[1] = dz[1];
  for (i = 2; i <= l; i++) {
     zi[i] = zi[i-1] + dz[i] ;
  }
  
  z[0] = zi[1]/2.;
  for (i = 1; i < l; i++) {
     z[i] = 0.5*(zi[i+1] + zi[i]);
  }

  return ;

} 
