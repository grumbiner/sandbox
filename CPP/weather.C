#include "icessmi.h"

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
      // ok
    }
    else {
      return WEATHER;
    }


}
