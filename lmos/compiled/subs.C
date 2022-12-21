#include "mvector.h"

float randnorm(float sdev) ;
float lognorm(float sdev) ;

float score(float toler, mvector<float> &delta, int start, int end) ;

void newpop(mvector<float> *regress, mvector<float> *sdevs,
            mvector<float> &best, mvector<float> &sd_best);

float evaluate(mvector<float> *matchups, int nparms, int ntrain, float toler, mvector<float> &tmp) ;

void make_vector(mvector<float> *matchups, int nparms, int start, int end, float toler, mvector<float> &best, mvector<float> &delta) ;

void display(mvector<float> *matchups, int nparms, int nobs, mvector<float> &best, mvector<float> &sd_best) ;

////////////////////////////////////////////////////////
void make_vector(mvector<float> *matchups, int nparms, int start, int end, float toler, mvector<float> &best, mvector<float> &delta) {
  for (int i = 0; i < delta.xpoints(); i++) {
    delta[i] = matchups[TERR][i+start] + best[0];
    for (int parm = 1; parm < nparms; parm++) { 
      delta[i] += best[parm]*matchups[parm][i+start];
    }
  }
    //for (int parm = 3; parm < nparms; parm++) { 
    //  delta[i] += best[1]*(matchups[1][i+start]-matchups[2][i+start]);
    //  delta[i] += best[2]*pow((matchups[1][i+start]-matchups[2][i+start]),2.)/10.0;
  return;
}
////////////////////////////////////////////////////////
void display(mvector<float> *matchups, int nparms, int nobs, mvector<float> &best, mvector<float> &sd_best) {
  int i, k, parm;
  float toler = 1./1.8;
  mvector<float> delta(nobs);

  printf("best genome: ");
    for (parm = 0; parm < best.xpoints(); parm++) {
      printf("%6.2f %7.3f  ",best[parm],sd_best[parm]);
    }
  printf("\n"); fflush(stdout);

  make_vector(matchups, nparms, 0, nobs, toler, best, delta);
  for (i = 0; i < nobs; i++) {
    printf("display %3d ",i);
    for (k = 0; k < 10; k++) {
      printf("%.2f ",matchups[k][i]);
    }

    printf(" delta  %6.2f \n",delta[i]);
  } 

  return;
}
///////////////////////////////////////////////////////////
// start-end, not just ntrain, return score
float evaluate(mvector<float> *matchups, int nparms, int ntrain, float toler, mvector<float> &tmp) {
  int end = ntrain, start = 0;
  mvector<float> delta(end - start + 1);

  make_vector(matchups, nparms, 0, ntrain, toler, tmp, delta);
  return (score(toler, delta, 0, ntrain));
}
////////////////////////////////////////////////////////
float score(float toler, mvector<float> &delta, int start, int end) {
// Score is to penalize the deltas:
//A)  if |delta| < toler, no penalty (obs are only to 1 F)
//1) else sum squared deviation
//2) else sum 4th power
  int i;
  double sumsq = 0.0, power = 2.;
  for (i = start; i <  end; i++) {
    //if (fabs(delta[i]) > toler) {
      sumsq += pow(delta[i],power);
    //}
  }
  return (float) pow(sumsq/((double)(end-start)), 1./power);
}
////////////////////////////////////////////////////////
void newpop(mvector<float> *regress, mvector<float> *sdevs, 
            mvector<float> &best, mvector<float> &sd_best)  {
  int np, nparms = best.xpoints();
  int memno, nmem = regress[0].xpoints();

  memno = 0;
  for (np = 0; np < nparms; np++) {
    regress[np][memno] = best[np];
    sdevs[np][memno]   = sd_best[np];
  }
  for (memno = 1; memno < nmem; memno++) {
    for (np = 0; np < nparms; np++) {
      regress[np][memno] = best[np] + randnorm(sd_best[np]);
      sdevs[np][memno]   = lognorm(sd_best[np])*sd_best[np];
    }
  }
  return;
}
//////////////////////////////////////////////////////////
float randnorm(float sdev) {
  float v1, v2, rsq, fac;

  do {
    v1 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v2 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v1 = 2.*v1 - 1.;
    v2 = 2.*v2 - 1.;
    rsq = v1*v1 + v2*v2;
  }
  while (rsq >= 1.0 || rsq == 0.0) ;

  fac = sqrt(-2.*log(rsq)/rsq);
  return v1*fac*sdev;

}
// note the 0.25 is a kludge addition, make it 1 for original
float lognorm(float sdev) {
  return exp(0.25*randnorm(sdev));
}
//////////////////////////////////////////////////////////
void hill_climb(mvector<float> *matchups, int nparms, int ntrain, float toler, mvector<float> &tmp, bool update) ;

void hill_climb(mvector<float> *matchups, int nparms, int ntrain, float toler, mvector<float> &best, bool update) {
  mvector<float> delta(ntrain), tmp2(best.xpoints());
  float stmp[best.xpoints()+1], ds[best.xpoints()], dsbest = 0.0;
  int i, j, jmax = -1;
  float perturb = 0.1;

  make_vector(matchups, nparms, 0, ntrain, toler, best, delta);
  stmp[0] = score(toler, delta, 0, ntrain);

  for (j = 0; j < best.xpoints(); j++) {
    tmp2 = best;
    tmp2[j] += perturb; // 'small' perturbation -- how to define/arrive at?
    make_vector(matchups, nparms, 0, ntrain, toler, tmp2, delta);
    
    stmp[j+1] = score(toler, delta, 0, ntrain);
    ds[j] = (stmp[j+1]-stmp[0]);
    if (fabs(ds[j]) > dsbest) {
      jmax = j;
      dsbest = ds[j];
    }
  }
  printf("j dsbest update %d %f %d\n",jmax, dsbest, update);
  //if (update) best[jmax] += 0.1*dsbest/fabs(dsbest); // smaller score is better
  if (update) best[jmax] += perturb;

}
