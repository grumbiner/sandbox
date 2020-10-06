#include <cstdio>
#include <stack>

#include "ncepgrids.h"
#include "avhrr.h"

int main(int argc, char *argv[]) {
  avhrrpt x, ref;
  FILE *fin, *foutlow, *fouthigh;
  int i = 0;
  global_ice<int> count;
  global_ice<float> rate, resol, sst;
  grid2<stack<avhrrpt> > data(count.xpoints(), count.ypoints() );

  global_12th<int> counthr;
  global_12th<float> ratehr, resolhr, ssthr;

  latpt ll;
  ijpt loc, hrloc;
  float flagsst = 265.0, flagresol = 55.*4;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }

  count.set(0);
  rate.set((float) 0.0);
  sst.set((float) 0.0);
  resol.set((float) 0.0);

  counthr.set(0);
  ratehr.set((float) 0.0);
  ssthr.set((float) 0.0);
  resolhr.set((float) 0.0);


// Loop over input file and distribute to stacks in a data grid
  while (!feof(fin) ) {

    fread(&x, sizeof(x), 1, fin);
    if (!feof(fin)) {i++; }
      else {break;}

    ll.lat = x.clat;
    ll.lon = x.clon;
    loc    = count.locate(ll);
    data[loc].push(x);

  }
  printf("i = %d\n",i);
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
       #ifdef VERBOSE
       printf("%3d %3d  %6.2f %6.2f  %4d ", loc.i, loc.j, ll.lat, ll.lon, count[loc]);
       #endif

       // Now pop through stack and do something with data for that point
       while (!data[loc].empty() ) {
          ref = data[loc].top();
          
          data[loc].pop();
          sst[loc] += ref.sst;
          tcount += 1;

          // put in secondary grid here -- average out in later loop
          ll.lat = ref.clat;
          ll.lon = ref.clon;
          hrloc = ssthr.locate(ll);
          ssthr[hrloc] += ref.sst;
          counthr[hrloc] += 1; 
       }
       sst[loc] /= tcount;
       #ifdef VERBOSE
         printf(" %f  %6.2f\n",rate[loc], sst[loc]);
         fflush(stdout);
       #endif
     }
     else {
       sst[loc] = flagsst;
     }
  }
  }
  printf("max number of observations = %d max rate per 100 km2 %e\n", 
                  count.gridmax(), rate.gridmax() );
  printf("avg number of observations = %d avg rate per 100 km2 %e\n", 
                  count.average(), rate.average() );
  printf("resolution max, min, avg, rms %e %e %e %e\n",resol.gridmax(flagresol),
            resol.gridmin(flagresol), resol.average(flagresol), resol.rms(flagresol) );


// run through and compute ssthr averages, rates, resolutions, and put in flag value
  for (loc.j = 0; loc.j < ssthr.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ssthr.xpoints(); loc.i++) {
    if (counthr[loc] == 0) {
      ssthr[loc] = flagsst;
      resolhr[loc] = flagresol;
    }
    else {
      ssthr[loc] /= counthr[loc];
      ratehr[loc] = 1.e8 * (double) counthr[loc] / counthr.cellarea(loc);
      resolhr[loc] = 10./sqrt(ratehr[loc]);
    }
  }
  }
  printf("max number of hr observations = %d max rate per 100 km2 %e\n", 
                  counthr.gridmax(), ratehr.gridmax() );
  printf("avg number of hr observations = %d avg rate per 100 km2 %e\n", 
                  counthr.average(), ratehr.average() );
  printf("hr resolution max, min, avg, rms %e %e %e %e\n",resolhr.gridmax(flagresol),
            resolhr.gridmin(flagresol), resolhr.average(flagresol), resolhr.rms(flagresol) );

//---------------------------------------------------------


  foutlow = fopen(argv[2],"w");
  sst.binout(foutlow);
  count.binout(foutlow);
  resol.binout(foutlow);
  fclose(foutlow);

  fouthigh = fopen(argv[3],"w");
  ssthr.binout(fouthigh);
  counthr.binout(fouthigh);
  resolhr.binout(fouthigh);
  fclose(fouthigh);

  palette<unsigned char> gg(19,65);

  count.scale();
  count.xpm("count.xpm",7,gg);
  rate.scale();
  rate.xpm("rate.xpm",7,gg);
  sst.scale(); 
  sst.xpm("sst.xpm",7,gg);

  counthr.scale();
  counthr.xpm("counthr.xpm",7,gg);
  resolhr.scale();
  resolhr.xpm("resolhr.xpm",7,gg);
  ssthr.scale(); 
  ssthr.xpm("ssthr.xpm",7,gg);

  return 0;
}
