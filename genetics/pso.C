#include <stdlib.h>
#include "ncepgrids.h"

#define SWARM 50

float ran1(void) ;
void gradfind(grid2<float> &x, fijpt &loc, fijpt &newloc) ;

int main(void) {
  FILE *fin;
  global_ice<float> sst;
  mvector<fijpt> position(SWARM);
  mvector<fijpt> velocity(SWARM);
  mvector<fijpt> pbest(SWARM);    // Best value seen by bird
  mvector<float> val_pbest(SWARM);
  fijpt gbest;                    // Best value seen by swarm
  fijpt tmpfij, dv;
  float val_gbest, tmp, tkelvin = 273.15; 
  int i, count = 0;
  float drag = (1.-0.3), alpha = 1.8, start_vel = 5.;
  latpt ll;
  ijpt loc;

  //read in data file 
  //initialize swarm positions, velocities, pbest, gbest
  //loop:
  //  compute energy parameters 
  //  update velocities and positions
  //  score
  //  test for pbest, gbest
  //endloop

// Read:
  fin = fopen("sst","r");
  sst.binin(fin);
  //sst -= 270.0;
  //sst *= sst;
  //for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  //for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
  //  ll = sst.locate(loc);
  //  if (fabs(ll.lat) > 60.0 && sst[loc] > tkelvin + 20) {
  //    printf("bizarre sst being reset -- %f %f, %f\n",ll.lat, ll.lon, sst[loc]);
  //    sst[loc] = tkelvin;
  //  }
  //}
  //}
 
  
// Initialize swarm positions, velocities, pbest, gbest, val_pbest, val_gbest 
  srand(1);
  val_gbest = 0.;
  for (i = 0; i < SWARM; i++) {
    //velocity[i].i = start_vel * (0.5 - rand()/(RAND_MAX + 1.0) );
    //velocity[i].j = start_vel * (0.5 - rand()/(RAND_MAX + 1.0) );
    velocity[i].i = start_vel * rand()/(RAND_MAX + 1.0) ;
    velocity[i].j = start_vel * rand()/(RAND_MAX + 1.0) ;
    velocity[i].k = 0.;
    position[i].i = sst.xpoints() / 2.; 
    position[i].j = sst.ypoints() / 2.; 
    //position[i].i = (float) sst.xpoints()* (float) rand()/(RAND_MAX + 1.0);
    //position[i].j = (float) sst.ypoints()* (float) rand()/(RAND_MAX + 1.0);
    position[i].k = 0.;
    pbest[i] = position[i];
    val_pbest[i] = sst[pbest[i]];
    if (val_pbest[i] > val_gbest) {
        val_gbest = val_pbest[i];
        gbest = position[i];
    }
  }
  printf("sst max = %f\n",sst.gridmax() );

  //while (val_gbest < (tkelvin + 35.0-270.)*(tkelvin + 35.0-270.) && count < 320) {
  while (val_gbest < (tkelvin + 35.0) && count < 320) {
    // compute energy parameters
    float tke = 0.; 
    float rms = 0., range = 0.;
    fijpt avg;  avg.i = 0.; avg.j = 0.; avg.k = 0.;
    for (i = 0; i < SWARM; i++) {
      tke += velocity[i].magnitude();
      avg += velocity[i];
      rms += velocity[i].i*velocity[i].i + velocity[i].j*velocity[i].j;
    }
    range = val_pbest.maximum() - val_pbest.minimum();
    avg.i /= (float) SWARM;
    avg.j /= (float) SWARM;
    rms /= (float) SWARM;
    rms = sqrt(rms);
    printf("%3d tke = %f, avg = %7.2f %7.2f, rms %6.2f  gbest: %6.2f range %f\n",
            count, tke, avg.i, avg.j, rms, val_gbest, range);
    // velocity[0].i, velocity[0].j, position[0].i, position[0].j);

    // Update positions:
    for (i = 0; i < SWARM; i++) {
      // Loop points around in longitude
      position[i].i += velocity[i].i;  
      while (position[i].i > sst.xpoints() - 1) position[i].i -= sst.xpoints();
      while (position[i].i < 0.) position[i].i += sst.xpoints();
  
      // Reflect pole-crossers across pole
      position[i].j += velocity[i].j;
      if (position[i].j > sst.ypoints() - 1) {
        position[i].j = (sst.ypoints() - 1)*2 - position[i].j;
        position[i].i += sst.xpoints()/2;
        while (position[i].i > sst.xpoints() - 1) position[i].i -= sst.xpoints();
        velocity[i].i = -velocity[i].i;
        //velocity[i].j = -velocity[i].j;
      } 
      if (position[i].j < 0) {
        position[i].j = -position[i].j;
        position[i].i += sst.xpoints()/2;
        while (position[i].i > sst.xpoints() - 1) position[i].i -= sst.xpoints();
        velocity[i].i = -velocity[i].i;
        //velocity[i].j = -velocity[i].j;
      }

//////// Update bests:
      if (sst[position[i]] > val_pbest[i]) {
        pbest[i] = position[i];
        val_pbest[i] = sst[pbest[i]];
        if (val_pbest[i] > val_gbest) {
            val_gbest = val_pbest[i];
            gbest = position[i];
        }
      }
      //printf("%d  %f %f  %f %f  %f\n",i, position[i].i, position[i].j, 
      //                                   velocity[i].i, velocity[i].j, 
      //                                   val_pbest[i]);
   
    
      // Update velocities -- personal best:
      //tmp    = ((float)alpha*ran1())* (1.-fabs(val_gbest - val_pbest[i])/range)  ;
      tmp    = ((float)alpha*ran1()) ;
      dv = position[i] ; dv -= pbest[i];
      dv.i *= tmp/10.;
      dv.j *= tmp/10.;
      velocity[i] += dv;
      // Update velocities -- global best:
      //tmp    = ((float)alpha*ran1()) * fabs(val_gbest - val_pbest[i])/range  ;
      tmp    = ((float)alpha*ran1())  ;
      dv = position[i] ; dv -= gbest;
      dv.i *= tmp/10.;
      dv.j *= tmp/10.;
      velocity[i] += dv;

      // Update velocities -- coulomb friction
      velocity[i].i *= drag;
      velocity[i].j *= drag;
      
      // Bound velocities some:
      while (velocity[i].i < -sst.xpoints()) { 
          velocity[i].i += (float) sst.xpoints() ;
      }
      while (velocity[i].i > sst.xpoints()) { 
          velocity[i].i -= (float) sst.xpoints() ;
      }
      while (velocity[i].j < -sst.ypoints()) { 
          velocity[i].j += (float) sst.ypoints() ;
      }
      while (velocity[i].j > sst.ypoints()) { 
          velocity[i].j -= (float) sst.ypoints() ;
      }
      

    } // end for
    // Hybridizing idea: do gradient finder 
    //for (i = 0; i < SWARM; i++) {
    //  gradfind(sst, position[i], tmpfij);
    //  if (sst[position[i]+tmpfij] > val_pbest[i]) {
    //    printf("gradient improvement %d %f %f  %f\n",i,val_pbest[i], sst[position[i]+tmpfij],
    //                         val_pbest[i] - sst[position[i]+tmpfij] );
    //    position[i] += tmpfij;
    //    pbest[i] = position[i];
    //    val_pbest[i] = sst[pbest[i]];
    //    if (val_pbest[i] > val_gbest) {
    //        val_gbest = val_pbest[i];
    //        gbest = position[i];
     //   }
     // }
    //}
    
    count += 1;
  }

  for (i = 0; i < SWARM; i++) {
    printf("%2d  %5.1f %5.1f  %6.2f %6.2f  %6.2f %6.2f  %6.2f\n",i, 
                  position[i].i, position[i].j, 
                  velocity[i].i, velocity[i].j, 
                  pbest[i].i, pbest[i].j, 
                  val_pbest[i]);
  }
  ll = sst.locate(gbest);
  printf("bests %6.2f %6.2f  %6.2f  %f %f\n",gbest.i, gbest.j, val_gbest,
          ll.lat, ll.lon);

  return 0;

}
float ran1(void) {
  return (float) rand()/(RAND_MAX + 1.0);
}
void gradfind(grid2<float> &x, fijpt &loc, fijpt &newloc) {
// gradient finder method for fine tuning of pretty good solutions
//  -- work in ij space for now
  fijpt tloc1, tloc2;
  float x0, xi, xj, dvdx, dvdy;

  tloc1 = loc;
  tloc2 = loc;
  tloc1.i += 1.;
  tloc2.j += 1.;
  x0 = x[loc];
  xi = x[tloc1];
  xj = x[tloc2];
  dvdx = (xi - x0);
  dvdy = (xj - x0);
  newloc.i = 2.*dvdx / sqrt(dvdx*dvdx + dvdy*dvdy);
  newloc.j = 2.*dvdy / sqrt(dvdx*dvdx + dvdy*dvdy);
  newloc.k = 0.;
  //if (sqrt(newloc.i*newloc.i + newloc.j*newloc.j) > 0.9875*2) {
  //  printf("newloc = %f %f\n",newloc.i, newloc.j);
  //}

}
