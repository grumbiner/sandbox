#include "ncepgrids.h"

void update(global_12th<float> &analy, global_12th<float> &imsice, global_12th<float> &filt, global_12th<unsigned char> &land, ijpt &loc, latpt &ll);

bool inside(latpt &loc, latpt &ll, latpt &ur) ;

int main(int argc, char *argv[]) {
  global_12th<float> analy, imsice, filt;
  global_12th<unsigned char> land;
  double add = 0.0, del = 0.0;
  FILE *fin;
  ijpt loc;
  latpt ll;
  int i;

  fin = fopen(argv[1],"r");
  analy.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  imsice.binin(fin);
  fclose(fin);
  fin = fopen(argv[3], "r");
  land.binin(fin);
  fclose(fin);

  for(i = 0; i < analy.xpoints()*analy.ypoints(); i++) {
    if (imsice[i] != 0 && analy[i] != 0) {
      filt[i] = analy[i];
    }
    else if (imsice[i] == 0 && analy[i] != 0) {
      loc.i = i % (analy.xpoints());
      loc.j = i / (analy.xpoints());
      ll = analy.locate(loc);
      if (ll.lat > 20 && land[i] == 0) {
        printf("delete %4d %4d %f %f  ims %f analy %f \n",loc.i, loc.j, ll.lat, ll.lon,
           imsice[loc], analy[loc]);
        del += analy.cellarea(loc);
        update(analy, imsice, filt, land, loc, ll);
      }
    }
    else if (imsice[i] != 0 && analy[i] == 0) {
      loc.i = i % (analy.xpoints());
      loc.j = i / (analy.xpoints());
      ll = analy.locate(loc);
      if (ll.lat > 20 && land[i] == 0) {
        printf("add %4d %4d %f %f  ims %f analy %f \n",loc.i, loc.j, ll.lat, ll.lon,
           imsice[loc], analy[loc]);
        add += analy.cellarea(loc);
        update(analy, imsice, filt, land, loc, ll);
      }
    }
    else {
      filt[i] = analy[i];
    }
  }
  printf("add = %e  delete = %e\n",add, del);

  fin = fopen(argv[4],"w");
  filt.binout(fin);
  fclose(fin);

  return 0;
}

void update(global_12th<float> &analy, global_12th<float> &imsice, global_12th<float> &filt, global_12th<unsigned char> &land, ijpt &loc, latpt &ll) {
  latpt ll1, ll2, ur1, ur2;
// ll1,ur1 -> mid atlantic US coast
// ll2,ur2 -> gulf of maine to east of newfoundland
  ll1.lat = 35;
  ll2.lat = 43;
  ll1.lon = 282;
  ll2.lon = 289;
  ur1.lat = 43;
  ur2.lat = 53.5;
  ur1.lon = 292;
  ur2.lon = 315;

  if (ll.lat < 35 || ll.lat > 53.5 || ll.lon < 282 || ll.lon > 315) return;

  if (inside(ll, ll1, ur1) || inside(ll, ll2, ur2) ) {
    if (imsice[loc] == 0 && analy[loc] != 0 && land[loc] == 0) {
      filt[loc] = 0;
    }
    else if (imsice[loc] != 0 && analy[loc] == 0 && land[loc] == 0) {
      filt[loc] = imsice[loc];
    }
  }
  
  return;
}
bool inside(latpt &loc, latpt &ll, latpt &ur) {
  if (loc.lat >= ll.lat && loc.lat <= ur.lat &&
      loc.lon >= ll.lon && loc.lon <= ur.lon) return true; 

  return false;
}
