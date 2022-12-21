#include "ncepgrids.h"
#include "resops.h"

void  check(grid2<float> &x) ;
void patch(grid2<float> &x, grid2<unsigned char> &land, fijpt &center, 
                int rangex, int rangey) ;

int main(int argc, char *argv[]) { 
  FILE *fin, *fout;
  grid2<float> aice(360,359), hi(360,359), hs(360,359), tsfc(360,359);
  grid2<float> tlat(360,359), tlon(360,359), sst(360,359);
  grid2<int> count(360,359);
  global_12th<float> analy;
  global_12th<unsigned char> gland;
  readin<float> *x, *y;
  ijpt itloc, loc;
  fijpt tloc;
  latpt ll;
  char fname[900];

  fin = fopen(argv[1],"r");
  aice.ftnin(fin);
  hi.ftnin(fin);
  hs.ftnin(fin);
  tsfc.ftnin(fin);
  tlat.ftnin(fin);
  tlon.ftnin(fin);
  sst.ftnin(fin);
  fclose(fin);

  fin = fopen(argv[2], "r");
  analy.ftnin(fin);
  fclose(fin);
  printf("analysis max min %f %f %f %f\n",analy.gridmax(), analy.gridmin(), analy.average(), analy.rms() );

  fin = fopen(argv[3], "r");
  gland.binin(fin);
  fclose(fin);
  printf("gland max min %d %d\n",gland.gridmax(), gland.gridmin() );

  printf(" aice "); check(aice);
  printf(" hi   "); check(hi);
  printf(" hs   "); check(hs);
  printf(" tsfc "); check(tsfc);
  printf(" tlat "); check(tlat);
  printf(" tlon "); check(tlon);
  printf(" sst  "); check(sst);

  x = new readin<float>(tlat, tlon);
  y = new readin<float>(tlat, tlon);
  count.set(0);   
  for (loc.j = 0; loc.j < aice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aice.xpoints(); loc.i++) {
    x->operator[](loc.i+loc.j*x->xpoints() ) = aice[loc];
  }
  }
  for (loc.j = 0; loc.j < analy.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < analy.xpoints(); loc.i++) {
      ll = analy.locate(loc);
      tloc = x->locate(ll);
  }
  }

//  for (loc.j = 3; loc.j < analy.ypoints(); loc.j++) {
//  for (loc.i = 0; loc.i < analy.xpoints(); loc.i++) {
//    if (gland[loc] < 1) {
//      ll = analy.locate(loc);
//      if (ll.lat > -40 && ll.lat < 30) continue;
//      if (ll.lat < -80) continue;
//      printf("%7.3f %7.3f  %5.3f  ",ll.lon, ll.lat, analy[loc]);
//      fflush(stdout);
//      itloc = x->locate(ll);
//      if (itloc.i < 0 || itloc.j < 0 || (itloc.i == 0 && itloc.j == 0) ) {
//        printf("error in mapping %d %d %f %f\n",itloc.i, itloc.j, ll.lat, ll.lon);
//      }
//      else {
//        printf(" %3d %3d %5.3f\n", itloc.i, itloc.j,  x->operator[](itloc) ); 
//      }
//      fflush(stdout);
//    }
//  }
//  }

  for (loc.j = 0; loc.j < aice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aice.xpoints(); loc.i++) {
      if (tlat[loc] > -40 && tlat[loc] < 30) continue;
      if (tlat[loc] < -80) continue;
    x->operator[](loc.i+loc.j*x->xpoints() ) = aice[loc];
    if (aice[loc] >= 0.0 && aice[loc] < 2. && tlat[loc] > -90. && tsfc[loc] > -90.) {
      ll = x->locate(loc);
      tloc = analy.locate(ll);
      printf("%7.3f %7.3f  %5.3f  %5.3f %5.3f  %7.3f %6.3f  %4.2f %3d  ",
              tlat[loc], tlon[loc],
              aice[loc], hi[loc], hs[loc], tsfc[loc], sst[loc], 
              analy[tloc], gland[tloc]);
      patch(analy, gland, tloc, 5, 2);
      printf("\n");
    }
  }
  }

  return 0;
}
void patch(grid2<float> &x, grid2<unsigned char> &land, fijpt &center, int rangex, int rangey) {
  ijpt loc, icenter;
  double sum = 0;
  int count = 0;
  icenter = center;
  for (loc.j = icenter.j - rangey; loc.j <= icenter.j + rangey; loc.j++) {
  for (loc.i = icenter.i - rangex; loc.i <= icenter.i + rangex; loc.i++) {
    printf("%4.2f %3d ", x[loc], land[loc]);
    if (land[loc] == 0) { 
      sum += x[loc];
      count++;
    }
  }
  } 
  if (count != 0) {
    printf("  %4.2f ",sum/count);
  }
  else {
    printf("  %4.2f ",0.0);
  }
    

  return;
}

void  check(grid2<float> &x) {
  float nonval;
  float universal = -99.;
  nonval = x.gridmax();
  printf(" %f %f %f %f \n",x.gridmax(nonval), x.gridmin(nonval), 
                           x.average(nonval), x.rms(nonval) ); 
// reflag nonvals to consistent number:
  for (int i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] == nonval) x[i] = universal;
  }

  return;
}
