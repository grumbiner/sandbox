#include "ncepgrids.h"

int depth = 0;
double area = 0.0;

void flood(ijpt &loc, llgrid<bool> &already, llgrid<float> &sst, float &tmpsst, const float &toler) ;

int main(int argc, char *argv[]) {
  global_quarter<float> sst;
  global_quarter<short int> land;
  global_quarter<bool> already; 
  short int landflag = 0;
  
  FILE *fin;
  ijpt loc, tloc;
  latpt ll;
  float tmpsst, toler = 0.5;
  int i;

  already.set(false);

  fin = fopen(argv[1],"r");
  sst.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  land.binin(fin);
  fclose(fin);
  for (i = 0; i < land.ypoints()*land.xpoints(); i++) {
    if (land[i] == landflag) already[i] = true;
  } 

  ll.lat = 89.85; ll.lon = 0.10;
  loc = sst.locate(ll);

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (!already[loc]) {
      tloc = loc;
      tmpsst = sst[loc];
      printf("%4d %4d  sst = %5.2f ", loc.i, loc.j, tmpsst);
      depth = 0;
      area  = 0;
      flood(tloc, already, sst, tmpsst, toler);
      printf("%6d %9.3f\n",depth, area/1e9);
      fflush(stdout);
    }
  }
  } 

  return 0;
}

void flood(ijpt &loc, llgrid<bool> &already, llgrid<float> &sst, float &tmpsst, const float &toler) {  
  //printf("depth %d flood loc = %d %d\n",depth, loc.i, loc.j); fflush(stdout);
  depth++;
 
  if (loc.i < 0 || loc.i > sst.xpoints() -1 || loc.j < 0 || loc.j > sst.ypoints() -1) {
    return;
  }

  if ((abs(sst[loc]-tmpsst) < toler) && (!already[loc]) ) {
    already[loc] = true;
    area += already.cellarea(loc);

    ijpt ip, jp, im, jm;
    ip = loc; ip.i += 1;
    flood(ip, already, sst, tmpsst, toler);
    jp = loc; jp.j += 1;
    flood(jp, already, sst, tmpsst, toler);
    im = loc; im.i -= 1;
    flood(im, already, sst, tmpsst, toler);
    jm = loc; jm.j -= 1;
    flood(jm, already, sst, tmpsst, toler);
  }    

  return;
}
