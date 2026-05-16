#include "grid_math.h"

// misc. scoring fragments

// Robert Grumbine

void score2(grid2<float> &fcst, grid2<float> &obs, grid2<float> &ref, float mindel,
            grid2<float> &land, grid2<float> &prob, mvector<float> &score) {
  float finterior[obs.xpoints()*obs.ypoints() ];
  float ointerior[obs.xpoints()*obs.ypoints() ];
  float deltainterior[obs.xpoints()*obs.ypoints() ];
  float fperiph[obs.xpoints()*obs.ypoints() ];
  float operiph[obs.xpoints()*obs.ypoints() ];
  int pcount=0, icount=0;
  int i; 
  int a11=0, a12=0, a21=0, a22=0;
  float level = 15./100.;
  float r2, xmean, ymean, sig2x, sig2y;
  ijpt loc;

  for (i = 0; i < obs.xpoints()*obs.ypoints() ; i++) {
// Extract the peripheral points
     if (( prob[i] > 0.0 + 6./78. && prob[i] < 1.0 - 6./78.) &&
         ( fcst[i] < 1.28 && fcst[i] >= 0) && 
         ( obs[i] < 1.28  && obs[i]  >= 0) &&
         ( land[i] <= 1.0 )        )   {
       fperiph[pcount]      = min(1.,fcst[i]);
       operiph[pcount]      = min(1.,obs[i]);
       pcount += 1;
     }
// Extract the interior points -- only if the delta from ref of one or the
//    other is greater than some minimum, say 0.05:
     if (
         ( fcst[i] < 1.28 && fcst[i] >= 0.15 ) && 
         ( obs[i]  < 1.28 && obs[i] >= 0.15  ) &&
         ( land[i] <= 1.0 )        )   {
       if ( fabs(fcst[i]-ref[i]) > mindel || fabs(obs[i]-ref[i]) > mindel) {
         finterior[icount]      = min(1.,fcst[i]);
         ointerior[icount]      = min(1.,obs[i]);
         deltainterior[icount]  = finterior[icount] - ointerior[icount];
         icount += 1;
       }
     }
// Find the record points:
     if ( prob[i] == 0.0 && obs[i] >= 0.15 && land[i] < 1.0 ) {
       loc.j = i/land.xpoints();
       loc.i = i % land.xpoints();
       //printf("record -- new ice at %3d %3d %5.3f\n",loc.i, loc.j, obs[i]);
     }
     if ( prob[i] == 1.0 && obs[i] <  0.15 && land[i] < 1.0 ) {
       loc.j = i/land.xpoints();
       loc.i = i % land.xpoints();
       //printf("record -- no ice at %3d %3d %5.3f\n",loc.i, loc.j, obs[i]);
     }
  }

// Score 0-3 are threat type scores -- based on peripheral points
  for (i = 0 ; i < pcount; i++) {
     if (fperiph[i] > level) {
       if (operiph[i] > level) {
         a11 += 1;
       }
       else {
         a12 += 1;
       }
     }
     else {
       if (operiph[i] > level) {
         a21 += 1;
       }
       else {
         a22 += 1;
       }
     }
  }

// FAR, POD, Threat, Q
   score[0] = (float) a12 / (float) (a11 + a12 ) ;
   score[1] = (float) a11 / (float) (a11 + a21 ) ;
   score[2] = (float) a11 / (float) (a11 + a21 + a12 ) ;
   score[3] = 1. - ((float) (a21*a12)) / (float) (a11*a22) ;

   if (icount == 0) return;
//Score 4 is the index of agreement:
   score[4] = iagree_(finterior,ointerior,icount);

// Score 5, 6, 7 are delta (forecast - obs) statistics -- mean, rmse, sdev:
   score[5] = sumx_(deltainterior, icount);
   score[6] = sumx2_(deltainterior, icount);
   score[7] = (icount*score[6] - score[5]*score[5]) / (icount*icount - icount);
   score[5] /= (float) icount;
   score[6] = sqrt(score[6]/(float) icount);
   score[7] = sqrt(score[7]); 
     
// score 8 is correlation (not squared):
   correl_(finterior, ointerior, icount, r2, xmean, ymean, sig2x, sig2y);
   score[8] = r2;

// score 9 is murphy skill score -- unusable in absence of mean:
//   score[9] = murphy(f, o, r, icount);
   score[9] = 0.;

  printf("%5d %5d ",pcount, icount);

  return;
}
