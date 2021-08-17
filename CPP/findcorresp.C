#include "metric.h"
#include "eta.h"

void findcorresp(GRID1<float> &x, GRID2<point3<float> > &corresp) ;
int main(void) {
  GRID1<float> x;
  GRID2<point3<float> > corresp;

  findcorresp(x, corresp);
  return 0;
}

void findcorresp(GRID1<float> &x, GRID2<point3<float> > &corresp) {
  //eta32<float> x;
  GRID1<point3<float> > etaloc, dloc_di, dloc_dj;
  GRID2<float> ice;
  //GRID2<point3<float> > corresp;
  GRID2<int> itcount;
  
  point3<float> di, dj, tmpi, tmpj;

  ijpt loc, merloc, ip, jp, locip, locjp;
  fijpt tijloc, tfl;
  latpt lloc, merll;

  float delta, dlat, dlon;
  float reflat = 45.0, reflon = -90; 
  int iter = 0, itmax = 20;
  float toler = 0.005; // tolerance in degrees
  float fi, fj;
  point3<float> fij;


// Set up locations grid
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    lloc = x.locate(loc);
    etaloc[loc] = lloc;
  }
  }

// Construct derivatives grid, assume for now that we aren't
//  interested in the edges.
  ip.i = 1;  ip.j = 0;
  jp.i = 0;  jp.j = 1;
  for (loc.j = 0; loc.j < x.ypoints()-1; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints()-1; loc.i++) {
    locip = loc; locip += ip;
    locjp = loc; locjp += jp;
    dloc_di[loc] = etaloc[locip]; dloc_di[loc] -= etaloc[loc];
    dloc_dj[loc] = etaloc[locjp]; dloc_dj[loc] -= etaloc[loc];
  }
  }

// Now start looking for the reference point -- use 2d newton method
  for (merloc.j = 0; merloc.j < ice.ypoints(); merloc.j++) {
  for (merloc.i = 0; merloc.i < ice.xpoints(); merloc.i++) {
     merll = ice.locate(merloc);
     reflat = merll.lat;
     reflon = merll.lon;
  
      if (reflon < 0) reflon += 360.;
      // If there's a prior guess, use it.  
      locip = merloc; locip -= ip;
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
      tijloc.i -= (-dlon*dloc_dj[loc].j + dlat*dloc_dj[loc].i) / delta;
      tijloc.j -= (-dlat*dloc_di[loc].i  + dlon*dloc_di[loc].j) / delta;
      loc.i = (int) (0.5 + tijloc.i);
      loc.j = (int) (0.5 + tijloc.j);
    
      fij = etaloc[loc];

      di = (tijloc.i - loc.i);
      dj = (tijloc.j - loc.j);
      tmpi = dloc_di[loc]; tmpi *= di;
      tmpj = dloc_dj[loc]; tmpj *= dj;
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

    //printf("all %3d %3d %2d  %f\n",merloc.i, merloc.j, iter, sqrt(dlat*dlat + dlon*dlon) );
    if (iter > itmax/2) {
      printf("%3d %3d %2d  ",merloc.i, merloc.j, iter);
      printf("%6.2f %7.2f  %6.2f %7.2f %f %f  %f %f  %f\n",
           lloc.lat, lloc.lon, reflat, reflon, dlat, dlon, tijloc.i, tijloc.j, sqrt(dlat*dlat + dlon*dlon) );
    }
    
  }
  }

// Being a newton search, but now only in 1 direction at a time.  The
//   2d search failed to converge at some points:
// Now start looking for the reference point -- use 1d newton method
  for (merloc.j = 0; merloc.j < ice.ypoints(); merloc.j++) {
  for (merloc.i = 0; merloc.i < ice.xpoints(); merloc.i++) {
     if (itcount[merloc] < itmax) continue;

     iter = 0;
     merll = ice.locate(merloc);
     reflat = merll.lat;
     reflon = merll.lon;
 
      if (reflon < 0) reflon += 360.;
      // If there's a prior guess, use it.
      locip = merloc; locip += ip;
      locjp = merloc; locjp += jp;
      if ( (merloc.i != ice.xpoints()-1) && (itcount[locip] < itmax)) {
         tfl = corresp[locip];
         loc.i = tfl.i;
         loc.j = tfl.j;
      }
      else {
        if ( (merloc.j != ice.ypoints()-1) && (itcount[locjp] < itmax)) {
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
      di = (tijloc.i - loc.i);
      dj = (tijloc.j - loc.j);
      tmpi = dloc_di[loc]; tmpi *= di;
      tmpj = dloc_dj[loc]; tmpj *= dj;
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
