#include "metric.h"
#include "mvector.h"
#include "points.h"
#include "geometry.C"

template<class T>
void region_assess(llgrid<T> &land, char *polyname, mvector<latpt> &locations, 
                   float north, float south, float east, float west,
                   float &minconc, char *argv[]) {
  FILE *fin;
  ijpt loc;
  latpt ll;
  int i, npts = locations.xpoints();
  grid2<mvector<float> > *section;
  llgrid<T> tmpsub, *conc, mask;
  double area = 0., allarea = 0., deland = 0.;
  palette<unsigned char> gg(19, 65);
  int nonmask = 0;

// Make a mask file -- exclude points that are land, or that are
//  outside our defined area of interest
  //printf("Entered the assess routine \n"); fflush(stdout);
  conc = new llgrid<T>(land.xpoints(), land.ypoints(), land.dlat, land.dlon, land.firstlat, land.firstlon);
  *conc = land;
  conc->subset(mask, north+2, south-2, east+2, west-2);

  if (east < 0) east += 360.;
  if (west < 0) west += 360.;

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    ll = mask.locate(loc); 
    if (ll.lon < 0.) ll.lon += 360.;

    allarea += mask.cellarea(loc);
    if (mask[loc] == 157) deland += mask.cellarea(loc);

    // != 0 means that you're inside the domain
    if (wn_PnPoly(ll, locations, npts ) == 0 ) {
      mask[loc] = 157;
    } 

    if (mask[loc] == 0) {
      nonmask += 1;
      area += mask.cellarea(loc);
    }
  }
  }

  //char tname[900], fname[900];
  //strncpy(fname, polyname,8);
  //sprintf(tname,"%s2.xpm",fname);
  //mask.xpm(tname,7,gg);

  printf("%7.2f %7.2f %7.2f %4d %4d  %s\n",area/1e9, deland/1e9, allarea/1e9, mask.xpoints(), mask.ypoints(), polyname);
  fflush(stdout);

  section = new grid2<mvector<float> > (mask.xpoints(), mask.ypoints());
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    (section->operator[](loc)).resize(1825);
  }
  } 

// Construct a big grid of vectors, which vectors contain the 5 year
//   time series of data
  for (i = 0; i < 1825; i++) {
    printf("i = %d\n",i); fflush(stdout);
    fin = fopen(argv[i+1],"r");
      conc->binin(fin); 
    fclose(fin); 
    conc->subset(tmpsub, north+2, south-2, east+2, west-2);
    for (loc.j = 0; loc.j < tmpsub.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < tmpsub.xpoints(); loc.i++) {
      section->operator[](loc)[i] = (float) tmpsub[loc];
    }
    }
  }
    
// Here is where the work would be done
//   For all points not-masked, ...
  for (loc.j = 0; loc.j < tmpsub.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmpsub.xpoints(); loc.i++) {
    section->operator[](loc)[i] = (float) tmpsub[loc];
    tmpsub[loc] = (unsigned char) (0.5 + section->operator[](loc).average() );
  }
  }
  char tname[900], fname[900];
  strncpy(fname, polyname,8);
  sprintf(tname,"%s.xpm",fname);
  tmpsub.xpm(tname,13,gg);
 
  return;
}
