#include "ncepgrids.h"
#include "buoy.h"

float latval(latpt &buoy, gaussian<float> &u) ;
float arcdis(latpt &lat1, latpt &lat2) ;
float diverg(gaussian<float> &u, gaussian<float> &v, latpt &buoyloc) ;
#define MAXLEN 2000

int main(int argc, char *argv[]) {
  FILE *finu, *finv, *finbuoy;
  gaussian<float> u(254), v(254);
  grid2_base<float> x(u.xpoints(), u.ypoints() );
  int n, nexpect = x.xpoints()*x.ypoints();
  int i1, i2, i3, i4;
  ijpt loc;
  latpt buoyloc;
  fijpt interp;
  float lat, lon;
  char tstring[2*MAXLEN];
  int id, j1, j2, j3, j4;
  float f1, f2, dist, dx, dy;

  finu = fopen(argv[1],"r");
  n = u.binin(finu);
  fclose(finu);
  if (n != nexpect) {
    printf("didn't read in expected points, only got %d of %d\n",n, 
               nexpect) ;
  }
  finv = fopen(argv[2],"r");
  n = v.binin(finv);
  fclose(finv);
  if (n != nexpect) {
    printf("didn't read in expected points, only got %d of %d\n",n, 
               nexpect) ;
  }

  finbuoy = fopen(argv[3],"r");
  while (!feof(finbuoy) ) {
    fgets(tstring, MAXLEN, finbuoy);
    sscanf(tstring,"%d %d %d %d %f %f %d %d %d %d %d %f %f %f %f %f\n",
         &i1, &i2, &i3, &i4, &lat, &lon,
         &id, &j1, &j2, &j3, &j4, &f1, &f2, &dist, &dx, &dy);

    buoyloc.lat =   lat;
    buoyloc.lon =   lon;
    //buoyloc.lat =   83.550;
    //buoyloc.lon =  205.226;
  
    float tmpu = latval(buoyloc, u);
    float tmpv = latval(buoyloc, v);
    printf("%3d %2d %2d %2d %6.3f %7.3f  %f %f %f %7.3f %7.3f %7.3f  %e\n", 
        i1, i2, i3, i4, lat, lon, dist, dx, dy, 
        sqrt(tmpu*tmpu+tmpv*tmpv), tmpu, tmpv, diverg(u, v, buoyloc)) ; 
  }

  return 0;
}

float latval(latpt &buoy, gaussian<float> &u) {
  float fx, fy, fxy, deltax, deltay;
  float v1, v2, v3, v4, dum;
  ijpt loc1, loc2, loc3, loc4;
  fijpt interp;

  interp = u.locate(buoy); //gives the i,j location corresponding to the buoy's lat-lon
  //printf("%f %f  ij %f %f\n",buoy.lat, buoy.lon, interp.i, interp.j);

  loc1.i = (int) interp.i;
  loc1.j = (int) interp.j; 
  if (loc1.i > u.xpoints() - 1 && u.iscyclicx()) {
    loc1.i -= u.xpoints() - 1 ;
  }
    
  loc2.i = loc1.i + 1;
  if (loc2.i > u.xpoints() - 1 && u.iscyclicx()) {
    loc2.i -= u.xpoints() - 1 ;
  }
  loc2.j = loc1.j;
    
  loc3.i = loc1.i;
  loc3.j = loc1.j + 1;
  if (loc3.i > u.xpoints() - 1 && u.iscyclicx()) {
    loc3.i -= u.xpoints() - 1 ;
  }

  loc4.i = loc1.i + 1;
  loc4.j = loc1.j + 1; 
  if (loc4.i > u.xpoints() - 1 && u.iscyclicx()) {
    loc4.i -= u.xpoints() - 1 ;
  }

  v1 = u[loc1];
  v2 = u[loc2];
  v3 = u[loc3];
  v4 = u[loc4];

  deltax = interp.i - loc1.i; if (deltax > 1) {
      cout << "wraparound, help!\n";
  }
  deltay = interp.j - loc1.j;  
  fxy = v3 + v2 - (v1 + v4);
  fy  = v3 - v1 - deltax*fxy;
  fx  = v2 - v1 - deltay*fxy;
  dum = v1 + deltax*fx + deltay*fy + deltax*deltay*fxy;

  //printf("corners %f %f %f %f  dx, dy %f %f, value %f\n",
  //                 v1, v2, v3, v4, deltax, deltay, dum);

  return dum;
}
float diverg(gaussian<float> &u, gaussian<float> &v, latpt &buoyloc) {
   ijpt l1, l2, l3, l4;
   float dy1, dy2, dx1, dx2;
   fijpt floc;
   latpt loc1, loc2, loc3, loc4;
   float delta;

   floc = u.locate(buoyloc);
   l1.i = (int) floc.i;
   l1.j = (int) floc.j;
   l2 = l1; l3 = l1; l4 = l1;
   l2.i += 1;
   l3.j += 1;
   l4.i += 1; l4.j += 1;
   
   loc1 = u.locate(l1);
   loc2 = u.locate(l2);
   loc3 = u.locate(l3);
   loc4 = u.locate(l4);

   dy1 = arcdis(loc1, loc3)*1000.; // convert to m
   dx1 = arcdis(loc1, loc2)*1000.; 
   dx2 = arcdis(loc3, loc4)*1000.;
   dy2 = arcdis(loc2, loc4)*1000.;

   delta = -dy1*(u[l1]+u[l3])/2. +
           -dx1*(v[l1]+v[l2])/2. +
            dy2*(u[l2]+u[l4])/2. +
            dx2*(v[l3]+v[l4])/2.;
   delta /= (dx1+dx2)/2 * (dy1+dy2)/2;
   //printf("dx1,2 dy %7.1f %7.1f %7.1f   %7.1f delta %e\n",
   //             dx1, dx2, dy1, dx2-dx1, delta);

  return delta;

}

float arcdis(latpt &lat1, latpt &lat2) {
  return arcdis_(lat1.lon, lat1.lat, lat2.lon, lat2.lat);
}
