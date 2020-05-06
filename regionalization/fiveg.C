#include <stdio.h>
#include <string.h>
#include "mvector.h"
#include "ncepgrids.h"

///////////////// GMT shorelines
#include "geometry.C"
int bcount = 0;

///////////// Utilities
// Sbr to get the next segment:
int getseas(mvector<latpt> &locations, float &west, float &east,
              float &north, float &south, char *name, FILE *fin) ;
template<class T>
void region_assess(llgrid<T> &land, char *polyname, mvector<latpt> &locations, 
                   float north, float south, float east, float west,
                   float &min_conc, char *argv[]) ;
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   ;

#define REGIONS 900
int main(int argc, char *argv[]) {
  FILE *fin;
  char name[900];
  int found;
  float min_conc, west, east, north, south;
  mvector<latpt> locations;
  global_12th<unsigned char> land;
  palette <unsigned char> gg(19,65);
  llgrid<unsigned char> mask;

  min_conc = atof(argv[1]);
  fin = fopen("globmap","r");
  land.binin(fin);
  fclose(fin);
  //land.xpm("land.xpm",13,gg);

  fin = fopen("sea_data","r");
// Note that in this version, we have name and perimeter simultaneously,
//   so can go sequentially through the input file
  found = 1;
  while (!feof(fin) && found > 0 ) {
    found = getseas(locations, west, east, north, south, name, fin);
    region_assess(land, name, locations, north, south, east, west, min_conc, argv);
  }

  return 0;
}


//////////////////////////////////////////////

int getseas(mvector<latpt> &locations, float &west, float &east,
              float &north, float &south, char *name, FILE *fin) {
// Varying from getseg, this is to get ascii-crafted segments
  int i, n, npts; 
  latpt llat;

  fgets(name, 900, fin);
  // needed to remove the carriage return, and pad for columnar output
  n = strlen(&name[0]);
  for (i = n-1; i < 21; i++) {
    name[i] = ' ';
  }
  name[21] = '\0';
  //printf("%s\n",name);
  fscanf(fin,"%d %f %f\n",&npts, &llat.lat, &llat.lon);
  //printf("%d %f %f\n",npts, llat.lat, llat.lon);
  locations.resize(npts);
  
  for (i = 0; i < npts; i++) {
    fscanf(fin,"%f %f\n",&llat.lat, &llat.lon);
    //printf("%f %f\n",llat.lat, llat.lon);
    if (llat.lon < 0) llat.lon += 360.;
    locations[i] = llat;
  }
  fflush(stdout);

  if (locations[0].lat != locations[npts-1].lat || 
      locations[0].lon != locations[npts-1].lon    ) {
    printf("error, last point must be same as first\n");
    fflush(stdout);
    return -1;
  }
  corners(locations, north, south, east, west);

  return npts;
}
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
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   {
  north = -95.0;
  south = +95.0;
  east = -400.0;
  west =  400.0;
  for (int i = 0; i < x.xpoints(); i++) {
    north = max(north, x[i].lat);
    south = min(south, x[i].lat);
    east  = max(east, x[i].lon);
    west  = min(west, x[i].lon);
  }  

  return ;
}
