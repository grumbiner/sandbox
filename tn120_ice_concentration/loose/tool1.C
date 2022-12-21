#include "ncepgrids.h"

#include "time_series.h"
#include "icessmi.h"

// Catchall demonstration/sample code for doing a number of utility
//   functions with ice analysis system
// You'll need to compile with -DCPLUS.  Various things are left as
//   legacy to the C versions of the code and you'll get those otherwise.

// Note: 3ssmi files need to be run through dd if=n3ssmi.YYYYMMDD of=work conv=swab
//   if you're on a linux box vs. a Cray or IBM-SP

// Note that for files before 18 Nov 1998, you'll need 'oldssmi'.  After that,
//   use 'ssmi' as the type for reading in the files.  This is declared in 
//   icessmi.h

int main(int argc, char *argv[]) {
   
  return 0;
}
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
  printf("oldssmi %lu\n",sizeof(oldssmi) );
  printf("   ssmi %lu\n",sizeof(ssmi)    );
  printf("ssmitmp %lu\n",sizeof(ssmi_tmp) );
  printf("team2_tables %lu\n",sizeof(ssmi_team2_tables) );
  printf("short_bufr   %lu\n",sizeof(ssmi_short_bufr) );
  printf("bufr_point   %lu\n",sizeof(ssmi_bufr_point) );
  printf("bufr_line    %lu\n",sizeof(ssmi_bufr_line ) );

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
               max(x.gridmax(), x.gridmax() ), rate);
  
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
  int diff_count = 0;

  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    if (fabs(y[loc] - x[loc]) > big) {
        diff_count += 1;
        ll = y.locate(loc);
        printf("Mismatch %7.3f %7.3f  %5.3f %5.3f %5.3d\n",ll.lat, ll.lon,
                       x[loc], y[loc], y[loc] - x[loc] );
      }
  }
  }
  return;
}
