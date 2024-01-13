#include "ncepgrids.h"

#define gravity 9.806

#define gridscale 0.20

#include "shared.C"

int main(void) {
  global_nth<short int> etopo2(30.);
  global_nth<float> bathy(gridscale), eta(gridscale);
  FILE *fin1;
  float flag = -99.0;

  fin1 = fopen("etopo2.flip","r");
  etopo2.binin(fin1);
  fclose(fin1);
//  printf("etopo2 max min %f %f\n",(float) etopo2.gridmax(), (float) etopo2.gridmin() );

//  grid2<short int> dbdb2(etopo2.xpoints(), etopo2.ypoints() + 1);
//  FILE *fin2;
//  fin2 = fopen("dbdb2.raw"  ,"r");
//  dbdb2.binin(fin2);
//  fclose(fin2);
//  printf("dbdb2  max min %f %f\n",(float) dbdb2.gridmax(), (float) dbdb2.gridmin() );

  average(etopo2, bathy);
  printf("bathy dlat dlon %f %f\n",bathy.dlat, bathy.dlon);

  global_nth<float> c2(gridscale), dx(gridscale), dy(gridscale), div(gridscale);
  mvector<float> etain (eta.xpoints()*eta.ypoints());
  mvector<float> etaout(eta.xpoints()*eta.ypoints());
  int i, iter;

  //c2  = bathy;
  c2.set((float) 4080.);
  c2 *= gravity;
  printf("max speed = %f\n",sqrt(c2.gridmax()) );

// Initialize with uniformly random field [-0.5,0.5)
  random(eta);
  eta -= 0.5;
  printf("etaout.xpoints %d\n",etaout.xpoints() );
  for (i = 0; i < etaout.xpoints(); i++) {
    etain[i] = eta[i];
  }
  printf("eta %f %f\n",eta.gridmax(), eta.gridmin() );
  show(eta);

  for (iter = 0; iter < 50; iter++) {
    // Iterations:
    gradients(eta, dx, dy, flag);
    //printf("dx %e %e\n",dx.gridmax(), dx.gridmin() );
    //printf("dy %e %e\n",dy.gridmax(), dy.gridmin() );
  
    dx *= c2;
    dy *= c2;
    divergence(dx, dy, div);
    //printf("div %e %e\n",div.gridmax(), div.gridmin() );
    for (i = 0; i < etaout.xpoints(); i++) {
      etaout[i] = div[i];
    }
    printf("norms %e %e ratio %e \n",etain.norm(), etaout.norm(), etaout.norm()/ etain.norm() );
    etaout *= etain.norm()/etaout.norm() ;
    //for (i = 0; i < etaout.xpoints(); i++) {
    //  if (etain[i] != 0.) {
    //    printf("%3d %3d %12.5e %12.5e %f\n",iter, i, etain[i], etaout[i], etaout[i]/etain[i]);
    //  }
    //}

    etain = etaout;
    for (i = 0; i < etaout.xpoints(); i++) {
      eta[i] = etain[i];
    } 
  // repeat
  }
  show(eta);

  return 0;
}

