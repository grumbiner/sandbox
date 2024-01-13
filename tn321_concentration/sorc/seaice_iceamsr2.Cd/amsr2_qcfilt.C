// Program for deriving sea ice concentrations from AMSR2 observations
// Regression to AMSR-E brightness temperatures provided by Walt Meier 4/2015
// Also update to weather filter
// Robert Grumbine 2 August 2017
//#include <stack>
//using namespace std;

#include "ncepgrids.h"
#include "amsr2.h"

bool hfok(amsr2_hrpt &hr) ;
bool lfok(amsr2_lrpt &lr) ;
float weather(double &t19v, double &t19h, double &t24v, double &t37v, double &t37h,
             double &t89v, double &t89h) ;
void amsr2_regress_to_amsre(double &v19, double &h19, double &v24, 
               double &v37, double &h37, double &v89, double &h89, float lat) ;

// Friends (accum, avg) below along with classes for them

////////////////////////////////////////////////////////////////////////////////

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
/////////////////////////////////////////////////////

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

}
// Regress amsr2 back to amsre
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

////////////////////////////////////////////////////////////////////////////////
/// Friends:
class amsr2_base {
  public :
    amsr2_spot spot;
  public :
    amsr2_base(void);
};
amsr2_base::amsr2_base() {
  spot.sccf = 0;
  spot.alfr = 0;
  spot.anpo = 0;
  spot.viirsq = 0;
  spot.tmbr   = 0;
}

//class amsr2_hr_accum : public amsr2_base {
//  don't inherit, we're doing a 'composed of' class
class amsr2_hr_accum {
  public :
    int count;
    amsr2_base hr[2];
  public :
    amsr2_hr_accum(void);
};
amsr2_hr_accum::amsr2_hr_accum() {
  count = 0;
}
class amsr2_lr_accum {
  public :
    int count;
    amsr2_base lr[12];
  public :
    amsr2_lr_accum(void);
};
amsr2_lr_accum::amsr2_lr_accum() {
  count = 0;
}
void hradd(grid2<amsr2_hr_accum> &nh_hr_accum, ijpt loc, amsr2_hrpt &hr);
void lradd(grid2<amsr2_lr_accum> &nh_lr_accum, ijpt loc, amsr2_lrpt &lr);
void hravg(grid2<amsr2_hr_accum> &nh_hr_accum);
void lravg(grid2<amsr2_lr_accum> &nh_lr_accum);
void hradd(grid2<amsr2_hr_accum> &nh_hr_accum, ijpt loc, amsr2_hrpt &hr) {
  nh_hr_accum[loc].count += 1;
  for (int i = 0; i < 2; i++) {
    nh_hr_accum[loc].hr[i].spot.sccf += hr.obs[i].sccf;
    nh_hr_accum[loc].hr[i].spot.alfr += hr.obs[i].alfr;
    nh_hr_accum[loc].hr[i].spot.anpo += hr.obs[i].anpo;
    nh_hr_accum[loc].hr[i].spot.viirsq += hr.obs[i].viirsq;
    nh_hr_accum[loc].hr[i].spot.tmbr += hr.obs[i].tmbr;
  }
  return;
}
void lradd(grid2<amsr2_lr_accum> &nh_lr_accum, ijpt loc, amsr2_lrpt &lr) {
  nh_lr_accum[loc].count += 1;
  for (int i = 0; i < 12; i++) {
    nh_lr_accum[loc].lr[i].spot.sccf += lr.obs[i].sccf;
    nh_lr_accum[loc].lr[i].spot.alfr += lr.obs[i].alfr;
    nh_lr_accum[loc].lr[i].spot.anpo += lr.obs[i].anpo;
    nh_lr_accum[loc].lr[i].spot.viirsq += lr.obs[i].viirsq;
    nh_lr_accum[loc].lr[i].spot.tmbr += lr.obs[i].tmbr;
  }
  return;
}
void hravg(grid2<amsr2_hr_accum> &nh_hr_accum) {
  ijpt loc;
  for (loc.j = 0; loc.j < nh_hr_accum.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nh_hr_accum.xpoints(); loc.i++) {
    if (nh_hr_accum[loc].count != 0) {
      for (int i = 0; i < 2; i++) {
        nh_hr_accum[loc].hr[i].spot.sccf /= nh_hr_accum[loc].count ;
        nh_hr_accum[loc].hr[i].spot.alfr /= nh_hr_accum[loc].count ;
        nh_hr_accum[loc].hr[i].spot.anpo /= nh_hr_accum[loc].count ;
        nh_hr_accum[loc].hr[i].spot.viirsq /= nh_hr_accum[loc].count ;
        nh_hr_accum[loc].hr[i].spot.tmbr /= nh_hr_accum[loc].count ;
      }
    }
  }
  }
  return;
}
void lravg(grid2<amsr2_lr_accum> &nh_lr_accum) {
  ijpt loc;
  for (loc.j = 0; loc.j < nh_lr_accum.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nh_lr_accum.xpoints(); loc.i++) {
    if (nh_lr_accum[loc].count != 0) {
      for (int i = 0; i < 12; i++) {
        nh_lr_accum[loc].lr[i].spot.sccf /= nh_lr_accum[loc].count ;
        nh_lr_accum[loc].lr[i].spot.alfr /= nh_lr_accum[loc].count ;
        nh_lr_accum[loc].lr[i].spot.anpo /= nh_lr_accum[loc].count ;
        nh_lr_accum[loc].lr[i].spot.viirsq /= nh_lr_accum[loc].count ;
        nh_lr_accum[loc].lr[i].spot.tmbr /= nh_lr_accum[loc].count ;
      }
    }
  }
  }
  return;
}
