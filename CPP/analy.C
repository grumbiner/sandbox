#include <cstdio>
#include <stack>
using namespace std;

#include "ncepgrids.h"
#include "amsr2.h"

bool hfok(amsr2_hrpt &hr) ;
bool lfok(amsr2_lrpt &lr) ;
float weather(double &t19v, double &t19h, double &t24v, double &t37v, double &t37h,
             double &t89v, double &t89h) ;
void amsr2_regress_to_amsre(double &v19, double &h19, double &v24, 
               double &v37, double &h37, double &v89, double &h89, float lat) ;

#include "amsr2_team2.C"

int main(int argc, char *argv[]) {
// Reading through file:
  amsr2head head;
  amsr2_spot s[12];
  amsr2_hrpt hr;
  amsr2_lrpt lr;
  northgrid<bool> ngrid;
  grid2<stack<amsr2_hrpt> > hrobs(ngrid.xpoints(), ngrid.ypoints());
  grid2<stack<amsr2_lrpt> > lrobs(ngrid.xpoints(), ngrid.ypoints());
  
  int i, nok_hf = 0, nok_lf = 0, nread = 0, nobs, nhigh = 0, nlow = 0;
  int nconc = 0;
  FILE *fin, *fout;
  float icecon;
  ijpt loc;
  latpt ll;

  printf("sizeof hr %d lr %d grid %d by %d = %d\n",sizeof(hr), sizeof(lr), 
        ngrid.xpoints(), ngrid.ypoints(), ngrid.xpoints()*ngrid.ypoints() );
  fflush(stdout);

// Algorithm Prep:
// For team2
  amsr_team2_tables arctic, antarctic;
  arctic.tbmfy.resize(n_atm, n_tb);
  arctic.tbmow.resize(n_atm, n_tb);
  arctic.tbmcc.resize(n_atm, n_tb);
  arctic.tbmthin.resize(n_atm, n_tb);
  arctic.pole = 'n';
  arctic_tables(arctic);
  lookuptable(arctic);

  antarctic.tbmfy.resize(n_atm, n_tb);
  antarctic.tbmow.resize(n_atm, n_tb);
  antarctic.tbmcc.resize(n_atm, n_tb);
  antarctic.tbmthin.resize(n_atm, n_tb);
  antarctic.pole = 's';
  antarctic_tables(antarctic);
  lookuptable(antarctic);
// End Team2 initialization

// Reading:
  fin = fopen("sat", "r");
  rewind(fin);

  //ll.lat = 20.0; ll.lon = 30.0;
  //loc = ngrid.locate(ll);
  //printf("loc %d %d for %f %f in: %d\n",loc.i, loc.j, ll.lat, ll.lon, ngrid.in(loc) );

  //ll.lat = 75.0; ll.lon = 30.0;
  //loc = ngrid.locate(ll);
  //printf("loc %d %d for %f %f in: %d\n",loc.i, loc.j, ll.lat, ll.lon, ngrid.in(loc) );

  //return 0;

  while (!feof(fin) && nread < 400e6) {
    loc.j = -1; loc.i = -1;
    fread(&head, sizeof(head), 1, fin);
    nobs = head.nspots;
    ll.lat = head.clat;
    ll.lon = head.clon;

    fread(&s[0], sizeof(amsr2_spot), nobs, fin);
    if (ll.lat < 25.0) continue;

    if (nobs == 12) { // low resolution spots
      nlow += 1;
      lr.head = head;
      for (i = 0; i < nobs; i++) { lr.obs[i] = s[i]; }
      if (lfok(lr)) { 
        loc = ngrid.locate(ll);
        if (lrobs.in(loc)) {
          nok_lf++; 
          lrobs[loc].push(lr);
        }
      }
    }
    if (nobs == 2) { // high resolution spots
      hr.head = head;
      nhigh += 1;
      for (i = 0; i < nobs; i++) { hr.obs[i] = s[i]; }
      if (hfok(hr)) { 
        loc = ngrid.locate(ll);
        if (hrobs.in(loc)) {
          nok_hf++; 
          hrobs[loc].push(hr);
        }
      }
    }

   if (hfok(hr) && lfok(lr) ) {
     icecon = nasa_team2(lr.obs[AMSR2_T19V].tmbr, lr.obs[AMSR2_T19H].tmbr, 
                         lr.obs[AMSR2_T24V].tmbr, 
                         lr.obs[AMSR2_T37V].tmbr, lr.obs[AMSR2_T37H].tmbr, 
                         hr.obs[AMSR2_T89V].tmbr,
                         hr.obs[AMSR2_T89H].tmbr, arctic, (float) ll.lat);
      if (icecon != 224.0 && icecon != 177.0) {
      nconc += 1;
      }
    }
                       
    if (nread % 10000 == 0) printf("nr = %d k\n",nread/1000); fflush(stdout);
    nread += 1;
  }
  fclose(fin);
  printf("nr = %d nread, # ok high freq = %d  low freq = %d\n",nread, nok_hf, nok_lf);

// now loop over vhres grid and see now many spots per cell we have:
  for (loc.j = 0; loc.j < ngrid.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ngrid.xpoints(); loc.i++) {
    if (!hrobs[loc].empty() || !lrobs[loc].empty() ) {
      ll = ngrid.locate(loc);
      printf("nhr %d nlr %d  loc: %d %d  %f %f\n",hrobs[loc].size(), lrobs[loc].size(), 
              loc.i, loc.j, ll.lat, ll.lon);
    }
  }
  }

  return 0;
}
bool hfok(amsr2_hrpt &hr) {
  bool tmp = true;
  if (hr.obs[0].tmbr > 285. || hr.obs[1].tmbr > 285.) tmp = false;
  if (hr.obs[0].tmbr > hr.obs[1].tmbr ) tmp = false;

  return tmp;
}
bool lfok(amsr2_lrpt &lr) {
  bool tmp = true;
// range test
  for (int i = 0; i < 12; i++) {
    if (lr.obs[i].tmbr > 285.) tmp = false;
  }
// polarization test
  for (int i = 0; i < 12; i+= 2) {
    if (lr.obs[i].tmbr > lr.obs[i+1].tmbr ) tmp = false;
  }

  return tmp;
}

// Make function to do the computations/filtering regarding weather.
// Isolate the decision to this, rather than the embedded structure
//   (in to nasa_team and team2) in the prior renditions of weather
//   filtering.
// Proximally prompted by the F-15 new filter, but makes sense for
//   any system
// Version for AMSR2

float weather(double &t19v, double &t19h, double &t24v, double &t37v, double &t37h,
             double &t89v, double &t89h) {
    float gr37, gr24;
    float grcrit_ice = -0.055, grcrit_water = +0.2412;
    float amsr2_gr37lim = 0.046;
    float amsr2_gr24lim = 0.045;

    gr37 = (t37v - t19v) / (t37v + t19v);
    gr24 = (t24v - t19v) / (t24v + t19v);
    if (gr37 < amsr2_gr37lim && gr24 < amsr2_gr24lim) {
      return 0;
    }
    else {
      return WEATHER;
    }

//    if (gr01 > grcrit_water && gr05 > grcrit_ice) {
//      return WEATHER;
//    }
//    else if (gr01 > grcrit_water) {
//      return 0.0;
//    }
//    else if (gr05 > grcrit_ice) {
//      return 1.0;
//    }
//    else {
//      return WEATHER;
//    }

//    return WEATHER;
}
void amsr2_regress_to_amsre(double &v19, double &h19, double &v24, 
                            double &v37, double &h37, double &v89, double &h89, float lat) {
  if (lat > 0) {
    v19 = v19 * 1.031 - 9.710;
    h19 = h19 * 1.001 - 1.104;
    v24 = v24 * 0.999 - 1.706;
    v37 = v37 * 0.997 - 2.610;
    h37 = h37 * 0.996 - 2.687;
    v89 = v89 * 0.989 + 0.677;
    h89 = h89 * 0.977 + 3.184;
  }
  else {
    v19 = v19 * 1.032 - 10.013;
    h19 = h19 * 1.000 - 1.320;
    v24 = v24 * 0.993 - 0.987;
    v37 = v37 * 0.995 - 2.400;
    h37 = h37 * 0.994 - 2.415;
    v89 = v89 * 0.975 + 4.239;
    h89 = h89 * 0.969 + 4.935;
  }

  return;
}
