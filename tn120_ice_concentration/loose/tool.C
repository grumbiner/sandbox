#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"
#include "icessmi.h"
#include "time_series.h"


// Catchall demonstration/sample code for doing a number of utility
//   functions with ice analysis system
// You'll need to compile with -DCPLUS.  Various things are left as
//   legacy to the C versions of the code and you'll get those otherwise.

// Note: 3ssmi files need to be run through dd if=n3ssmi.YYYYMMDD of=work conv=swab
//   if you're on a linux box vs. a Cray or IBM-SP

// Note that for files before 18 Nov 1998, you'll need 'oldssmi'.  After that,
//   use 'ssmi' as the type for reading in the files.  This is declared in 
//   icessmi.h

void concbarvsbarconc(metricgrid<ssmi> &x, mvector<int> &delta) {
  ijpt loc;
  delta.resize(513);
  delta = 0;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    delta[256 + x[loc].bar_conc - x[loc].conc_bar] += 1;
  }
  }

  return;
} 

// Show sizes of utility types used in processing 
void sizes (void) {
  printf("oldssmi %d\n",sizeof(oldssmi) );
  printf("   ssmi %d\n",sizeof(ssmi)    );
  printf("ssmitmp %d\n",sizeof(ssmi_tmp) );
  printf("team2_tables %d\n",sizeof(ssmi_team2_tables) );
  printf("short_bufr   %d\n",sizeof(ssmi_short_bufr) );
  printf("bufr_point   %d\n",sizeof(ssmi_bufr_point) );
  printf("bufr_line    %d\n",sizeof(ssmi_bufr_line ) );

  return;
}
// print out a grid filled with dummy values
template <class T>
void dummy(grid2<T> &x, T flag, FILE *fout) {
  x.set((T)flag);
  x.binout(fout);
  fclose(fout);
  return;
}
// Compute auto-covariances at a given lag for a set of grids
float autocovary(mvector<grid2<float> > &x, int lag, ijpt &ref) {
  time_series<float> y;
  int i;
  for (i = 0 ; i < x.xpoints(); i++) {
    y[i] = x[i][ref];
  }
  return y.autocovary(lag);
}
// Print lat-long of corners, edges, central cross
template <class T>
void locations(mvector<T> &x) {
  ijpt loc;
  latpt ll;
  printf("Corners\n");
  loc.i = 0; loc.j = 0; ll = x.locate(loc);
  printf("ll lat lon %f %f\n",ll.lat, ll.lon);
  loc.i = x.xpoints()-1; loc.j = 0; ll = x.locate(loc);
  printf("lr lat lon %f %f\n",ll.lat, ll.lon);
  loc.i = 0; loc.j = x.ypoints()-1; ll = x.locate(loc);
  printf("ul lat lon %f %f\n",ll.lat, ll.lon);
  loc.i = x.xpoints()-1; loc.j = x.ypoints()-1; ll = x.locate(loc);
  printf("ur lat lon %f %f\n",ll.lat, ll.lon);

  printf("\n\nleft edge\n");
  loc.i = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    ll = x.locate(loc);
    printf("%d %d  lat lon %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  printf("\n\nright edge\n");
  loc.i = x.xpoints() - 1;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    ll = x.locate(loc);
    printf("%d %d  lat lon %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  printf("\n\nbottom edge\n");
  loc.j = 0;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    printf("%d %d  lat lon %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  printf("\n\ntop edge\n");
  loc.j = x.ypoints() - 1;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    printf("%d %d  lat lon %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }

  printf("\n\nCentral cross\n");
  printf("\n\nhorizontal \n");
  loc.j = x.ypoints()/2;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    printf("%d %d  lat lon %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  printf("\n\nvertical \n");
  loc.i = x.xpoints()/2;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    ll = x.locate(loc);
    printf("%d %d  lat lon %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }

  return;
}

int ages(grid2<unsigned char> &x) {
  mvector<int> age(365);
  ijpt loc;
  int oldest;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    age[x[loc] ] += 1;
  }
  }
  for (int i = 0; i < age.xpoints(); i++){
    if (age[i] != 0) {
      printf("age %3d  count %3d\n",i,age[i]);
      oldest = i;
    }
  }
  return oldest;
  // maybe show xpm map?
}


void histogram(grid2<unsigned char> &age, mvector<int> &dist) {
  int i;
  int ncats = dist.xpoints();
  int npts = age.xpoints() * age.ypoints();

  dist = 0;

  for (i = 0; i < npts; i++) {
    if (age[i] < ncats) dist[age[i] ] += 1;
  }

  for (i = 0; i < ncats; i++) {
    if (dist[i] != 0) printf("%3d  %d\n",i,dist[i]);
  }

  return;
}


void obscounts(grid2<int> &x, char *fname) {
  palette<unsigned char> h(25);
  int i, rate;

  rate = x.gridmax();
  rate = 255 / rate;
  printf("Max number of obs, %d, color rate %d\n",
               x.gridmax(), rate);
  
  for (i = 0; i < 25; i++) {
    h.set_color(i, rate*i, rate*i, rate*i);
  }

  x.xpm(fname,1,h);
  return;
}

// loop over the grid and print out the differences if they're large
void large_delta(metricgrid<float> &x, metricgrid<float> &y, float big) {
  ijpt loc;
  latpt ll;
  int diff_count;

  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    if (fabs(y[loc] - x[loc]) > big) {
        diff_count += 1;
        ll = x.locate(loc);
        printf("Mismatch %7.3f %7.3f  %5.3f %5.3f %5.3d\n",ll.lat, ll.lon,
                       x[loc], y[loc], y[loc] - x[loc] );
      }
  }
  }
  return;
}

//  Conditional averaging


// Return the lat-long position of a given i,j coordinate
// Robert Grumbine
// 28 June 1999

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> n;
  char **x = NULL;
  ijpt loci;
  latpt locl;

  loci.i = strtol(argv[1], x, 10);
  loci.j = strtol(argv[2], x, 10);
  
  locl = n.locate(loci);
  printf("%3d %3d %7.3f %7.3f\n", loci.i, loci.j, locl.lat, locl.lon);

  return 0;
}

template <class T>
void xpmice(metricgrid<T> &newice, metricgrid<T> &land, char *fname) {
  ijpt x;
  latpt y;
  palette<unsigned char> gg(19, 65);
// Print out xpm files for graphic comparisons
  for (x.j = 0; x.j < newice.ypoints() ; x.j++) {
    for (x.i = 0; x.i < newice.xpoints() ; x.i++) {
       if (land[x] == LAND) {
         newice[x] = 0;
       }
       else if (land[x] == COAST) {
         newice[x] = 0;
       }
       if (newice[x] == NO_DATA) {
         newice[x] = 0;
       }
       else if (newice[x] > MAX_ICE) {
         newice[x] = 2;
       }
       else {
         newice[x] = 4 + min((unsigned char)100,newice[x])/7;
       }
    }
  }
  newice.xpm(&fname[0], 1, gg);

  return ;
}



////zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
// This will treat 3 frequencies as R, G, B and then show a map thereof.
//   Use flags from icessmi for argument values.
void rgb(metricgrid<ssmi> &x, int r, int g, int b, char *name) {
  grid2<int> red(x.xpoints(), x.ypoints());
  grid2<int> green(x.xpoints(), x.ypoints());
  grid2<int> blue(x.xpoints(), x.ypoints());
  ijpt loc;
  getfield(x, red, r);
  getfield(x, green, g);
  getfield(x, blue, b);
  return;
}
// Donmap -- make an 80x60 ASCII display of the ice concentrations about
//   a given i,j
void donmap(metricgrid<ssmi> &x, ijpt &center, FILE *fout) {
  grid2<char> omap(79, 59);
  ijpt loc;
  // see donmap.c, remember to flip i,j
  return;
}

// Compute grid to grid deltas ... will want to think about delta
//   in the case of a flag value
// Many delta-computing programs to work from
template<class T>
void delta(metricgrid<T> &x, metricgrid<T> &y, metricgrid<T> &del) {
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    del[loc] = x[loc] - y[loc];
  }
  }
  return 0;
}

// Compute the area and extent of a sea ice grid
//   note that declaring y a metricgrid doesn't work
// Will also want to manage flag values
// Many examples of area/extent computation
void area_extent(metricgrid<float> &x, metricgrid<unsigned char> &land, 
                 float &area, float &extent) {
  float scale = 1;
  ijpt loc;

  if (x.gridmax() > 3) scale = 0.01;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > MAX_ICE*scale) x[loc] = 0;
    if (x[loc] > 100*scale && x[loc] <= MAX_ICE*scale) x[loc] = 1.0/scale;
  }
  } 
  area = x.integrate()*scale;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 0) x[loc] = 1.0;
  }
  }
  extent = x.integrate(); 

  return;
}


int main2(int argc, char *argv[]) {
  GRIDTYPE<float> ingrid;
  GRIDTYPE<float> outgrid;
  GRIDTYPE<unsigned char> land;
  ijpt loc;
  int i, j;
  FILE *fin;
  float mul;

  fin = fopen(argv[1],"r");
  ingrid.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  land.binin(fin);
  fclose(fin);

  if (ingrid.average() < 3.) {
    mul = 100.;
  }
  else {
    mul = 1.;
  }
  outgrid =  ingrid * mul;
  for (loc.j = 0; loc.j < outgrid.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < outgrid.xpoints(); loc.i++) {
    if (outgrid[loc] > 128 || land[loc] > (unsigned char) 100) {
       outgrid[loc] = 0.0;
    }
  }
  }

  fin = fopen(argv[3],"w");
  outgrid.binout(fin);
  fclose(fin);
  
  return 0;
}

int main3(int argc, char *argv[]) {
  fijpt ll, ur;
  ijpt ill, iur;
  latpt loc1, loc2;
  northhigh<unsigned char> north;
  southhigh<unsigned char> south;
  psgrid<unsigned char> tmp;
  bool ok;
  palette<unsigned char> gg(19, 65);
  FILE *fin;

  loc1.lon = atof(argv[1]);
  loc1.lat = atof(argv[2]);
  loc2.lon = atof(argv[3]);
  loc2.lat = atof(argv[4]);
  fin = fopen("nland.new", "r");
  north.binin(fin);
  fclose(fin);
  fin = fopen("sland.new", "r");
  south.binin(fin);
  fclose(fin);

  if (loc1.lat > 0. && loc2.lat > 0.) {
    ll = north.locate(loc1);
    ur = north.locate(loc2);
    ok = north.in(ll) && north.in(ur);
  }
  else if (loc1.lat < 0. && loc2.lat < 0.) {
    ll = south.locate(loc1);
    ur = south.locate(loc2);
    ok = south.in(ll) && south.in(ur);
  }
  else {
    printf("Cannot deal with divided hemispheres on input points\n");
  }

  if (ok) {
    printf("Corners %7.2f %7.2f  %7.2f %7.2f\n", min(ll.i,ur.i), min(ll.j,ur.j), 
                                                 max(ur.i,ll.i), max(ur.j,ll.j)  );
    ill = ll;
    iur = ur;
    tmp.subset(north, ill, iur);
    tmp.xpm("a.xpm", 12, gg);
  }
  else {
    printf("Corner(s) falls outside region\n");
  }

  return 0;
}
