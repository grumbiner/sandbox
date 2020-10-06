#include "metric.h"
#include "ncepgrids.h"
#include "resops.h"

//GRID1 is the input grid
//GRID2 is the target (output) grid
//  find the location on the input grid which corresponds to the location 
//  on the output grid
//Derived/descended from the NARR analysis for ice from GLERL to eta32 grid
//Robert Grumbine 23 Dec 2015

void findcorresp(readin<float> &x, grid2<float> &lat, grid2<float> &lon, 
                 global_12th<point3<float> > &corresp) ;
int main(int argc, char *argv[]) {
// the rtofs grid, as a readin(resops) grid
  readin<float> *x;
  grid2<float> lat(4500,3298), lon(4500,3298), saltin(4500,3298);

  global_12th<point3<float> > corresp;

  ijpt loc, tloc;
  FILE *fin;

// Read in the data:
  float tlon, tlat, tsalt;
  fin = fopen(argv[1],"r");
  for (loc.j = 0; loc.j < saltin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < saltin.xpoints(); loc.i++) {
    fscanf(fin, "%f %f %f\n",&tlon, &tlat, &tsalt);
    lat[loc] = tlat;
    lon[loc] = tlon;
    saltin[loc] = tsalt;
  }
  }
  fclose(fin);
// now create the resops grid version
  x = new readin<float> (lat, lon);
  printf("nx ny = %d %d\n",x->xpoints(), x->ypoints() );
  for (loc.j = 0; loc.j < x->ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x->xpoints(); loc.i++) {
    x->operator[](loc) = saltin[loc];
  }
  }

  printf("about to call findcorresp\n"); fflush(stdout);
  findcorresp(*x, lat, lon, corresp);
  return 0;
}

void findcorresp(readin<float> &x, grid2<float> &lat, grid2<float> &lon, 
                  global_12th<point3<float> > &corresp) {
  readin<point3<float> > etaloc(lat, lon);
  grid2<point3<float> >  dloc_di(lat.xpoints(), lon.ypoints()), dloc_dj(lat.xpoints(), lon.ypoints());
  global_12th<int> itcount;
  
  ijpt loc, merloc, ip, jp, locip, locjp;
  fijpt tijloc, tfl, tmpi, tmpj;
  latpt lloc, merll;

  float delta, dlat, dlon;
  float reflat = 45.0, reflon = -90; 
  int iter = 0, itmax = 20;
  float toler = 0.005; // tolerance in degrees
  point3<float> fij;

  printf("entered findcorresp\n"); fflush(stdout);

// Set up locations grid
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    lloc = x.locate(loc);
    etaloc[loc] = lloc;
  }
  }
  printf("created locations grid\n"); fflush(stdout);

// Construct derivatives grid, assume for now that we aren't
//  interested in the edges.
  ip.i = 1;  ip.j = 0;
  jp.i = 0;  jp.j = 1;
  for (loc.j = 0; loc.j < x.ypoints()-1; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints()-1; loc.i++) {
    locip = loc ; locip += ip;
    locjp = loc ; locjp += jp;
    dloc_di[loc] = etaloc[locip]; dloc_di[loc] -= etaloc[loc];
    dloc_dj[loc] = etaloc[locjp]; dloc_dj[loc] -= etaloc[loc];
  }
  }
  printf("created derivatives grid\n"); fflush(stdout);

// Now start looking for the reference point -- use 2d newton method
  for (merloc.j = 0; merloc.j < corresp.ypoints(); merloc.j++) {
  for (merloc.i = 0; merloc.i < corresp.xpoints(); merloc.i++) {
    printf("looping on merloc = %d %d  ",merloc.i, merloc.j); fflush(stdout);
     merll = corresp.locate(merloc);
    printf("ll = %f %f \n",merll.lat, merll.lon); fflush(stdout);
     reflat = merll.lat;
     reflon = merll.lon;
  
      if (reflon < 0) reflon += 360.;
      if (reflon > 360.) reflon -= 360.;
      // If there's a prior guess, use it.  
      locip = merloc ; locip -= ip;
      if (merloc.i != 0 && (itcount[locip] < itmax) ) {
         tfl = corresp[locip];
         loc.i = tfl.i;
         loc.j = tfl.j;
      }
      else {
        loc.i = x.xpoints()/2;
        loc.j = x.ypoints()/2;
      }
      
    tijloc.i = loc.i;
    tijloc.j = loc.j;
    fij = etaloc[loc];
    lloc.lat = fij.j; lloc.lon = fij.i;
    dlat = reflat - lloc.lat;
    dlon = reflon - lloc.lon;
    if (dlon < -360.) dlon += 360.;
    if (dlon > 360.) dlon -= 360.;
  
// Now begin the iteration:
    iter = 0;
    while (iter < itmax && (fabs(dlat) > toler || fabs(dlon) > toler) ) {
      delta = dloc_di[loc].i * dloc_dj[loc].j - dloc_di[loc].j*dloc_dj[loc].i;
    printf("iteration = %d %d %d %f\n",iter, merloc.i, merloc.j, delta); fflush(stdout);
    if (delta == 0.0) break;
    printf("delta = %f\n",delta); fflush(stdout);
      tijloc.i -= (-dlon*dloc_dj[loc].j + dlat*dloc_dj[loc].i) / delta;
      tijloc.j -= (-dlat*dloc_di[loc].i + dlon*dloc_di[loc].j) / delta;
      loc.i = (int) (0.5 + tijloc.i);
      loc.j = (int) (0.5 + tijloc.j);
    
      fij = etaloc[loc];
      tmpi = dloc_di[loc]; tmpi *= (tijloc.i - loc.i);
      fij += tmpi;
      tmpj = dloc_dj[loc]; tmpj *= (tijloc.j - loc.j);
      fij += tmpj;
      lloc.lat = fij.j; lloc.lon = fij.i;
      dlat = reflat - lloc.lat;
      dlon = reflon - lloc.lon;
      if (dlon < -360.) dlon += 360.;
      if (dlon > 360.) dlon -= 360.;
   
      iter += 1;
    }
    corresp[merloc] = tijloc;
    itcount[merloc] = iter;

    printf("all %3d %3d %2d  %f %f  %f\n",merloc.i, merloc.j, iter, tijloc.i, tijloc.j, sqrt(dlat*dlat + dlon*dlon) ); fflush(stdout);
    if (iter > itmax/2) {
      printf("%3d %3d %2d  ",merloc.i, merloc.j, iter);
      printf("%6.2f %7.2f  %6.2f %7.2f %f %f  %f %f  %f\n",
           lloc.lat, lloc.lon, reflat, reflon, dlat, dlon, tijloc.i, tijloc.j, sqrt(dlat*dlat + dlon*dlon) ); fflush(stdout);
    }
    
  }
  }

// Being a newton search, but now only in 1 direction at a time.  The
//   2d search failed to converge at some points:
// Now start looking for the reference point -- use 1d newton method
  printf("starting newtonian search\n"); fflush(stdout);
  for (merloc.j = 0; merloc.j < corresp.ypoints(); merloc.j++) {
  for (merloc.i = 0; merloc.i < corresp.xpoints(); merloc.i++) {
     if (itcount[merloc] < itmax) continue;

     iter = 0;
     merll = corresp.locate(merloc);
     reflat = merll.lat;
     reflon = merll.lon;
 
      if (reflon < 0) reflon += 360.;
      // If there's a prior guess, use it.
      locip = merloc ; locip += ip;
      locjp = merloc ; locjp += jp;
      if ( (merloc.i != corresp.xpoints()-1) && (itcount[locip] < itmax)) {
         tfl = corresp[locip];
         loc.i = tfl.i;
         loc.j = tfl.j;
      }
      else {
        if ( (merloc.j != corresp.ypoints()-1) && (itcount[locjp] < itmax)) {
           tfl = corresp[locjp];
           loc.i = tfl.i;
           loc.j = tfl.j;
        }
        else {
          loc.i = x.xpoints()/2;
          loc.j = x.ypoints()/2;
        }
      }

    tijloc.i = loc.i;
    tijloc.j = loc.j;
    fij = etaloc[loc];
    lloc.lat = fij.j; lloc.lon = fij.i;
    dlat = reflat - lloc.lat;
    dlon = reflon - lloc.lon;
    if (dlon < -360.) dlon += 360.;
    if (dlon > 360.) dlon -= 360.;

    while (iter < itmax && (fabs(dlat) > toler || fabs(dlon) > toler) ) {
      if (fabs(dlon) > fabs(dlat) )  {
        tijloc.i += dlon / dloc_di[loc].i;
      }
      else {
        tijloc.j += dlat / dloc_dj[loc].j;
      }
      loc.i = (int) (0.5 + tijloc.i);
      loc.j = (int) (0.5 + tijloc.j);

      fij = etaloc[loc];
      tmpi = dloc_di[loc]; tmpi *= (tijloc.i - loc.i);
      tmpj = dloc_dj[loc]; tmpj *= (tijloc.j - loc.j);
      fij += tmpi;
      fij += tmpj;
      lloc.lat = fij.j; lloc.lon = fij.i;
      dlat = reflat - lloc.lat;
      dlon = reflon - lloc.lon;
      if (dlon < -360.) dlon += 360.;
      if (dlon > 360.) dlon -= 360.;

      iter += 1;
    }
    corresp[merloc] = tijloc;
    itcount[merloc] = iter;
    printf("all %3d %3d %2d  %f\n",merloc.i, merloc.j, iter, sqrt(dlat*dlat + dlon*dlon) );
    if (iter > itmax/2) {
      printf("%3d %3d %2d  ",merloc.i, merloc.j, iter);
      printf("%6.2f %7.2f  %6.2f %7.2f %f %f  %f %f  %f\n",
           lloc.lat, lloc.lon, reflat, reflon, dlat, dlon, tijloc.i, tijloc.j, sqrt(dlat*dlat + dlon*dlon) );
    }


  }
  }


  return ;
}
