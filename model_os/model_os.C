#define nu 0
#define lambda 1
#define C        2.99792458e8 // m/s
#define h_planck 6.626065957e-34  // Planck constant (J*s)
#define k_b      1.3806488e-23 // Boltzman constant, J/K

#include "mvector.h"

void planck(float Tb, mvector<float> &intensity, float delta, int type) ;
void planck(float Tb, mvector<float> &intensity, float delta, int type) {
  float i;
  float tmp1 = h_planck*delta/k_b/Tb;
  float tmp2 = (2*h_planck/C/C)*delta*delta*delta;

  if (type == nu) { // working in frequency
    for (i = 1; i < intensity.xpoints(); i++) {
      //intensity[i] = (2*h_planck/C/C) / (exp(h_planck*i*delta/k_b/Tb) - 1) * 
      //      (i*delta)*(i*delta)*(i*delta);
      
      //intensity[i] = tmp2*(i*i*i) / (exp(h_planck*i*delta/k_b/Tb) - 1) ;
      //intensity[i] = tmp2*(i*i*i) / (exp(tmp1*i) - 1) ;
      intensity[i] = (i*i*i) / (exp(i*tmp1) - 1);
    }
    intensity *= tmp2;
  }

}

int main(void) {
  mvector<float> earth(1e7), sun(1e7);
  float delta = 3.e8; // Hz
  int type = nu;
  double sum_earth = 0, sum_sun = 0;

  planck( 288, earth, delta, type);
  planck(5600, sun,   delta, type);
  for (int i = 1; i < earth.xpoints(); i++) {
    sum_earth += earth[i];
    sum_sun   += sun[i];
  }
  sum_earth *= delta;
  sum_sun   *= delta;
  printf("%e %e  %e\n",sum_earth, sum_sun, 5.67e-8*288.*288.*288.*288.);
  return 0;
 
  for (int i = 1; i < earth.xpoints(); i++) {
    //if (earth[i] > 1e-25) {
    //  printf("%e %e %e %e\n",C/(i*delta), earth[i], sun[i], sun[i]/earth[i]);
    //}
    //else {
      //printf("%e %e %e\n",C/(i*delta), earth[i]/sum_earth, sun[i]/sum_sun);
      printf("%e %e %e\n",(i*delta), earth[i], sun[i]);
    //}
  }

  return 0;
}


//class sun : {
//  public:
//    sun(void);
//};
//sun::sun(void) {
//  Sc = 1367;
//  Tb = 5600;
//}
//
//some objects/devices/modules:
//
//Sun (Sc(nu))
//Celestial Mechanics (earth/sun/moon/...)
//
//Heterosphere -- dynamics/physics/chemistry
//Homosphere -- ditto
//Aerosols
//Clouds
//
//Ocean
//Sea ice
//Land ice
//Land
//Hydrology, rivers, aquifers, ...
