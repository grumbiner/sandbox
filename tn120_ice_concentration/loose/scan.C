#include <cstdio>
#include <stack>

#include "ncepgrids.h"
// Scan ssmi tank info for sea ice-related, vs. avhrr
#include "ssmi.h"

int main(int argc, char *argv[]) {
  bufr_line x;
  bufr_point ref;
  FILE *fin, *foutlow;
  int index = 0, i;
  global_quarter<int> count;
  global_quarter<float> rate, resol, t19v, t37v, t85v, t22v;
  grid2<stack<bufr_point> > data(count.xpoints(), count.ypoints() );

  //llgrid<int>  count(360, 180, -1.0, 1.0, 0.5, 89.5);
  //llgrid<float> rate(360, 180, -1.0, 1.0, 0.5, 89.5);
  //llgrid<float>resol(360, 180, -1.0, 1.0, 0.5, 89.5);
  //llgrid<float> t19h(360, 180, -1.0, 1.0, 0.5, 89.5);
  //llgrid<float> t37h(360, 180, -1.0, 1.0, 0.5, 89.5);
  //llgrid<float> t85h(360, 180, -1.0, 1.0, 0.5, 89.5);
  //llgrid<float> t22v(360, 180, -1.0, 1.0, 0.5, 89.5);
  //grid2<stack<bufr_point> > data(360, 180);


  latpt ll;
  ijpt loc, hrloc;
  float flagresol = 55.*4;
  float flagt19v = 125.0, flagt37v = 120.0, flagt85v = 130.0;
  float flagt19h =  70.0, flagt37h =  80.0, flagt85h = 150.0;
  float flagt22v = 125.0;
  float flagmax = 343.15; // 70 C, 158 F
  
  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }

  count.set(0);
  rate.set((float) 0.0);
  t85v.set((float) 0.0);
  t37v.set((float) 0.0);
  t19v.set((float) 0.0);
  t22v.set((float) 0.0);
  resol.set((float) 0.0);

  //printf("size of bufr line: %d\n",sizeof(bufr_line) );
  //printf("size of bufr point: %d\n",sizeof(bufr_point) );
  //fflush(stdout);
  printf("nx, ny, prod = %d %d  %d\n",count.xpoints(), count.ypoints(), count.xpoints() * count.ypoints() );
  fflush(stdout);

// Loop over input file and distribute to stacks in a data grid
  while (!feof(fin) ) {
    if ((index % 5000) == 0) printf("index = %d\n",index); fflush(stdout);

    fread(&x, sizeof(x), 1, fin);
    if (!feof(fin)) {index++; }
      else {break;}

    for (i = 0; i < NSCANS; i++) {
      ll.lat = x.full[i].latitude;
      ll.lon = x.full[i].longitude;
      loc    = count.locate(ll);
      data[loc].push(x.full[i]);
    }
  }
  printf("index = %d\n",index);
  fclose(fin);

// Loop over the data grid and parcel out to the target grids:
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
     if (!data[loc].empty() ) {
       int tcount = 0;
       count[loc] = data[loc].size();
       rate[loc] = 1.e8 * (double) count[loc] / count.cellarea(loc);
       resol[loc] = 10./sqrt(rate[loc]);
       ll = count.locate(loc);
       #ifdef VERBOSE2
       printf("%3d %3d  %6.2f %6.2f  %4d ", loc.i, loc.j, ll.lat, ll.lon, count[loc]);
       #endif

       // Now pop through stack and do something with data for that point
       while (!data[loc].empty() ) {
          ref = data[loc].top();
          
          data[loc].pop();
          if (ref.t85v < flagmax &&
              ref.t37v < flagmax &&
              ref.t19v < flagmax &&
              ref.t22v < flagmax ) {
            t85v[loc] += ref.t85v;
            t37v[loc] += ref.t37v;
            t19v[loc] += ref.t19v;
            t22v[loc] += ref.t22v;
            tcount += 1;
          }
          else {
            printf("overmax %7.3f %7.3f %7.3f %7.3f\n", 
               ref.t85v, ref.t37v, ref.t19v, ref.t22v); fflush(stdout);
          }

          // put in secondary grid here -- average out in later loop
          ll.lat = ref.latitude;
          ll.lon = ref.longitude;
       }
       if (tcount != 0) {
         t85v[loc] /= tcount;
         t37v[loc] /= tcount;
         t22v[loc] /= tcount;
         t19v[loc] /= tcount;
         #ifdef VERBOSE2
           printf(" %f  %6.2f\n",rate[loc], t85v[loc]);
           fflush(stdout);
         #endif
       }
       else { // had data, but all was bad
         t85v[loc] = flagt85v;
         t37v[loc] = flagt37v;
         t19v[loc] = flagt19v;
         t22v[loc] = flagt22v;
       }
     }
     else { // had no data
       t85v[loc] = flagt85v;
       t37v[loc] = flagt37v;
       t19v[loc] = flagt19v;
       t22v[loc] = flagt22v;
     }
  }
  }

  printf("max number of observations = %d max rate per 100 km2 %7.3f\n", 
                  count.gridmax(), rate.gridmax() );
  printf("avg number of observations = %d avg rate per 100 km2 %7.3f\n", 
                  count.average(), rate.average() );
  printf("resolution max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",
                  resol.gridmax(flagresol), resol.gridmin(flagresol), 
                  resol.average(flagresol), resol.rms(flagresol) );

  printf("t85v max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",
            t85v.gridmax(flagt85v), t85v.gridmin(flagt85v), 
            t85v.average(flagt85v), t85v.rms(flagt85v) );
  printf("t37v max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",
            t37v.gridmax(flagt37v), t37v.gridmin(flagt37v), 
            t37v.average(flagt37v), t37v.rms(flagt37v) );
  printf("t19v max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",
            t19v.gridmax(flagt19v), t19v.gridmin(flagt19v), 
            t19v.average(flagt19v), t19v.rms(flagt19v) );
  printf("t22v max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",
            t22v.gridmax(flagt22v), t22v.gridmin(flagt22v), 
            t22v.average(flagt22v), t22v.rms(flagt22v) );


//---------------------------------------------------------


  foutlow = fopen(argv[2],"w");
  t85v.binout(foutlow);
  count.binout(foutlow);
  resol.binout(foutlow);
  fclose(foutlow);

  palette<unsigned char> gg(19,65);

  count.scale();
  count.xpm("count.xpm",7,gg);
  rate.scale();
  rate.xpm("rate.xpm",7,gg);

  t85v.scale(); 
  t85v.xpm("t85v.xpm",7,gg);
  t37v.scale(); 
  t37v.xpm("t37v.xpm",7,gg);
  t19v.scale(); 
  t19v.xpm("t19v.xpm",7,gg);
  t22v.scale(); 
  t22v.xpm("t22v.xpm",7,gg);

  return 0;
}
