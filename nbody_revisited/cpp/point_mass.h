#include <math.h>
#include "mvector.h"

// Astronomical point masses class -----------
typedef double dp ;

class astronomy {
  public:
  static const dp m_earth = 5.9722e24;
  static const dp m_sun   = 1.98847e30;
  static const dp au      = 1.49597870700e11;
  static const dp ly      = 9.46e15;
  static const dp parsec  = 3.26e0*ly;
  static const dp G       = 6.6743e-11;
  static const dp mean_solar_day = 86400.;
};

class point_mass : public mvector<dp> {
  public:
    mvector<dp> x, u, a;
    dp m, k;
  // rather than create and destroy these billions of times, keep a 
  //   scratch space along with the point_mass
  private:
    mvector<dp> scratch1;

  public:
// point mass stuff:
  point_mass();
  point_mass(point_mass & );
  void init_loc(dp &, dp &, dp &);
  void init_vel(dp &, dp &, dp &);
  void update_loc(dp &) ;
  void update_vel(dp &) ;
  void show(dp &);
  dp ke(void) ;
  dp dist(point_mass &) ;
  dp dist(mvector<dp> &) ;

// The astronomical parts:
  dp kepler(point_mass &) ;
  void gravity(point_mass &) ;
};
point_mass::point_mass(void) {
  this->x.resize(3);
  this->u.resize(3);
  this->a.resize(3);
  this->scratch1.resize(3);
  this->x = 0.;
  this->u = 0.;
  this->a = 0.;
  this->m = 0.;
  this->k = 0.;
}
point_mass::point_mass(point_mass &pt) {
  printf("copying a pointmass\n");
  this->x = pt.x ;
  this->u = pt.u ;
  this->a = pt.a ;
  this->m = pt.m ;
  this->k = pt.k ;
}
void point_mass::init_loc(dp &loc1, dp &loc2, dp &loc3) {
  this->x[0] = loc1;
  this->x[1] = loc2;
  this->x[2] = loc3;
}
void point_mass::init_vel(dp &u1, dp &u2, dp &u3) {
  this->u[0] = u1;
  this->u[1] = u2;
  this->u[2] = u3;
}
void point_mass::update_loc(dp &dt) {
  scratch1 = this->u;
  scratch1 *= dt;
  this->x += scratch1;
// For acceleration:
  scratch1 = this->a;
  scratch1 *= 0.5*dt*dt;
  this->x += scratch1;
}
void point_mass::update_vel(dp &dt) {
  scratch1 = this->a;
  scratch1 *= dt;
  this->u += scratch1;
  this->a = 0.;
}
void point_mass::show(dp &l) {
  printf("%f %f %f %f %f %f %f \n",
    this->x[0]/l, this->x[1]/l, this->x[2]/l, 
    this->u[0], this->u[1], this->u[2], this->m);     
}
dp point_mass::ke(void) {
  return(0.5*this->m*(this->u[0]*this->u[0] + this->u[1]*this->u[1] + this->u[2]*this->u[2]));
}
dp point_mass::dist(mvector<dp> &y) {
  scratch1 = y;
  scratch1 -= this->x;
  dp dist = sqrt(scratch1[0]*scratch1[0] + scratch1[1]*scratch1[1] + scratch1[2]*scratch1[2]);
  return dist;
}
dp point_mass::dist(point_mass &y) {
  scratch1 = 0.0;
  scratch1 = y.x;
  scratch1 -= this->x;
  dp dist = sqrt(scratch1[0]*scratch1[0] + scratch1[1]*scratch1[1] + scratch1[2]*scratch1[2]);
  return dist;
} 
dp point_mass::kepler(point_mass &y) {
  dp d = this->dist(y);
  return sqrt(y.k / d); 
}
void point_mass::gravity(point_mass &y) {
  dp d, accel;

  d = this->dist(y);
  accel = y.k / d/d/d;

  scratch1 = y.x;
  scratch1 -= this->x;
  scratch1 *= accel;
  this->a += scratch1;
}

