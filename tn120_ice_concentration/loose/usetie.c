#include <stdio.h>
#include <math.h>

/* Routine to compute tie points and pass them back in 'c', a pointer to
   floats */
/* Bob Grumbine 6 June 1994 */

void usetie(float *c, int ihem)
{
/* SSM/I Tie points of Jan 13, 1992, without thin ice */
  float tb19hw[2] = { 99.8,  99.8};
  float tb19vw[2] = {177.3, 177.3};
  float tb19hf[2] = {239.5, 239.5};
  float tb19vf[2] = {249.7, 249.7};
  float tb19hm[2] = {202.3, 202.3};
  float tb19vm[2] = {221.3, 221.3};
  float tb37vw[2] = {202.5, 202.5};
  float tb37vf[2] = {244.2, 244.2};
  float tb37vm[2] = {184.6, 184.6};

/* SSM/I New ice tie points Jan 13, 1992 */
  float tb19hn[2] = {190.8, 190.8};
  float tb19vn[2] = {250.0, 250.0};
  float tb37vn[2] = {246.6, 246.6};

  float dw[2], df[2], dm[2], dn[2];
  float sw[2], sf[2], sm[2], sn[2];
 
  dw[0] = tb19vw[ihem] - tb19hw[ihem];
  df[0] = tb19vf[ihem] - tb19hf[ihem];
  dm[0] = tb19vm[ihem] - tb19hm[ihem];
  dn[0] = tb19vn[ihem] - tb19hn[ihem];

  sw[0] = tb19vw[ihem] + tb19hw[ihem];
  sf[0] = tb19vf[ihem] + tb19hf[ihem];
  sm[0] = tb19vm[ihem] + tb19hm[ihem];
  sn[0] = tb19vn[ihem] + tb19hn[ihem];
  
  dw[1] = tb37vw[ihem] - tb19vw[ihem];
  df[1] = tb37vf[ihem] - tb19vf[ihem];
  dm[1] = tb37vm[ihem] - tb19vm[ihem];
  dn[1] = tb37vn[ihem] - tb19vn[ihem];
 
  sw[1] = tb37vw[ihem] + tb19vw[ihem];
  sf[1] = tb37vf[ihem] + tb19vf[ihem];
  sm[1] = tb37vm[ihem] + tb19vm[ihem];
  sn[1] = tb37vn[ihem] + tb19vn[ihem];

  c[0] = dm[0]*dw[1] - dm[1]*dw[0];
  c[1] = dm[1]*sw[0] - dw[1]*sm[0];
  c[2] = dw[0]*sm[1] - dm[0]*sw[1];
  c[3] = sm[0]*sw[1] - sm[1]*sw[0];
  c[4] = df[0]*(dm[1]-dw[1]) + dw[0]*(df[1]-dm[1]) + dm[0]*(dw[1]-df[1]);
  c[5] = df[1]*(sm[0]-sw[0]) + dw[1]*(sf[0]-sm[0]) + dm[1]*(sw[0]-sf[0]);
  c[6] = df[0]*(sw[1]-sm[1]) + dw[0]*(sm[1]-sf[1]) + dm[0]*(sf[1]-sw[1]);
  c[7] = sf[1]*(sw[0]-sm[0]) + sw[1]*(sm[0]-sf[0]) + sm[1]*(sf[0]-sw[0]);
  c[8] = df[1]*dw[0] - df[0]*dw[1];
  c[9] = dw[1]*sf[0] - df[1]*sw[0];
 c[10] = sw[1]*df[0] - dw[0]*sf[1];
 c[11] = sf[1]*sw[0] - sf[0]*sw[1];

  return;

} 
