// Astronomical point masses class -----------
#include "point_mass.h"

//!------------------ Main program ----------------------------------
int main(void) {
  int nbody;
  point_mass *system;
  mvector<dp> r0, ke0;
  mvector<dp> momentum(3), com(3), tmp(3), zero_vec(3);
 
  int isun, iearth, ijupiter;
  dp ratio, dt;
  int step, freq, yy;
  int i, j, k;
  
  dp zero = 0.0, totmass;
  dp l0 = astronomy::au;
  dp tmp_f;

  i     = 0;
  isun  = 0;
  iearth = 1;
  ijupiter = 2;

  nbody = 3;
  freq  = 8640 *4;
  ratio = 8640.*4; // fraction of a day per time step

  dt = astronomy::mean_solar_day/ratio;

// Solar system initialization --------------------------------------------
  r0.resize(nbody);
  ke0.resize(nbody);
  com = zero;
  momentum = zero;
  zero_vec = zero;

  system = new point_mass[nbody+1];

  system[isun].m = astronomy::m_sun;
  system[isun].k = astronomy::m_sun*astronomy::G;
  system[isun].init_loc(zero, zero, zero);
  system[isun].init_vel(zero, zero, zero);

  i = 1;
  system[i].m = astronomy::m_earth * 1.000;
  system[i].k = astronomy::G * system[i].m;
  tmp_f = l0;
  system[i].init_loc(zero, tmp_f, zero);
  tmp_f = system[i].kepler(system[isun]);
  system[i].init_vel(tmp_f, zero, zero);

  i = 2;
  system[i].m = astronomy::m_earth * 317.8;
  //system[i].m = astronomy::m_earth * 0.0;
  system[i].k = astronomy::G * system[i].m;
  tmp_f = l0*5.2;
  system[i].init_loc(tmp_f, zero, zero);
  tmp_f = -system[i].kepler(system[isun]) * ( 1. - system[i].m/(system[iearth].m+system[isun].m));
  system[i].init_vel(zero, tmp_f, zero);

  // update earth's motion for jupiter's contribution:
  //dp epsi = system[ijupiter].m/system[isun].m;
  //system[iearth].u *= sqrt(1. - system[ijupiter].m/system[isun].m);
  //system[iearth].u *= (1. + epsi/2.);

// Find center of mass and the momentum of the non-sun bodies:
  for (i = 1; i < nbody; i++) {
    tmp = system[i].u;
    tmp *= system[i].m;
    momentum += tmp;
    tmp = system[i].x;
    tmp *= system[i].m;
    com      += tmp;
  }

// update sun s.t. solar system has 0 momentum, 0,0,0 center of mass:
  i = isun;
  system[isun].x -= com ;
  system[isun].u -= momentum ;
  system[isun].x /= system[isun].m;
  system[isun].u /= system[isun].m;
  // check that we were successful in zeroing out the system c.o.m and momentum:
  tmp  = system[i].x;
  tmp *= system[i].m;
  com      += tmp;
  tmp  = system[i].u;
  tmp *= system[i].m;
  momentum += tmp;
  printf("initial system com %e %e %e\n",com[0], com[1], com[2]);
  printf("initial system momentum %e %e %e\n",momentum[0], momentum[1], momentum[2]);

// Compute ke0, r0, tke for all bodies
  dp tke = 0.;
  for (i = 0; i < nbody; i++) {
    //vs. c.o.m: 
    r0[i] = system[i].dist(zero_vec);
    // vs. the sun
    //r0[i] = system[i].dist(system[isun]);
    ke0[i] = system[i].ke();
    tke += ke0[i];
  }
  printf("tke initial: %e\n",tke);
// Apportionment:
  for (i = 0; i < nbody; i++) {
    printf("%d  %f\n",i,ke0[i]/tke);
  }

// Now that sun has moved, the keplerian initial velocity is slightly different. 
// for best precition, should update velocities (which then requires update to solar
// motion as well to keep system momentum = 0

// END Solar System Initialization -------------------------------------------------

// approx 18 years per minute, full Earth-Sun-Jupiter system, dt = 2.5 seconds
// g++, MacOS, -O2
  int ypm  = 18;  // dt = 2.5 seconds --> false earth recession ~3.4 microAU/year, Jupiter ~1/7 microAU/year
  int nmin = 4;
  //for (  yy = 0;   yy < ypm*nmin; yy++) {
  for (  yy = 0;   yy < 100; yy++) {
  for (step = 0; step < ratio*366; step++) {

    if ( (step)%freq == 0) {
      i = iearth;
      j = 2;
      printf("time %f %f %f  %f %f  %f  %f %f %f  %f\n",
        yy+step*dt/astronomy::mean_solar_day/366.,
// Earth:
        system[i].x[0]/l0, system[i].x[1]/l0,
        system[i].u[0], system[i].u[1],
        1.e6*(system[i].dist(zero_vec) - r0[i])/astronomy::au,
        //1.e6*(system[i].dist(system[isun]) - r0[i])/astronomy::au,
// Jupiter:
        system[j].x[0]/l0, system[j].x[1]/l0,
        1.e6*(system[j].dist(zero_vec) - r0[j])/astronomy::au,
// Sun:
        1.e6*(system[isun].dist(zero_vec) - r0[isun])/astronomy::au 
      );
    }

    // run j,k from 0 to nbody to have full action/reaction system
    for (j = 0; j < nbody; j++) {
      //following commented line is for nailing the sun down. then j,k run from 1
      //system[j].gravity(system[isun]);
      for (k = 0; k < nbody; k++) {
        if (k != j) {
          system[j].gravity(system[k]);
        }
      }
    }

    for (j = 0; j < nbody; j++) {
      system[j].update_loc(dt);
      system[j].update_vel(dt);
    }

  } // end step
  } // end yy

  i = iearth;
  j = 2;
  printf("time %f  %f %f  %f %f  %f  %f %f %f  %f\n",
    -1+yy+step*dt/astronomy::mean_solar_day/366.,
    system[i].x[0]/l0, system[i].x[1]/l0,
    system[i].u[0], system[i].u[1],
    1.e6*(system[i].dist(zero_vec) - r0[i])/astronomy::au,
    //vs. sun: 1.e6*(system[i].dist(system[isun]) - r0[i])/astronomy::au,

    system[j].x[0]/l0, system[j].x[1]/l0,
    1.e6*(system[j].dist(zero_vec) - r0[j])/astronomy::au, 
    //1.e6*(system[j].dist(system[isun]) - r0[j])/astronomy::au, 

    1.e6*(system[isun].dist(zero_vec) - r0[isun])/astronomy::au 
  );

// Close out with some integral checks:
  printf("\nsystem c.o.m position, velocity:");
  com = 0.;
  momentum = 0.;
  totmass  = 0.;
  dp kefin = 0.;
  for (i = 0; i < nbody; i++) {
    totmass += system[i].m;
    tmp  = system[i].x;
    tmp *= system[i].m;
    com += tmp;
    tmp  = system[i].u;
    tmp *= system[i].m;
    momentum += tmp;
    kefin += system[i].ke();
  }
  com /= totmass; 
  printf("com %e %e %e\n",com[0], com[1], com[2]);
  momentum /= totmass;
  printf("velocity %e %e %e\n",momentum[0], momentum[1], momentum[2]);
  printf("ke init %e final %e relative change %e\n",tke, kefin, (tke-kefin)/tke);

  return 0;
}
