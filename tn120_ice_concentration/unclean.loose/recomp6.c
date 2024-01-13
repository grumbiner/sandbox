#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "oldicessmi.h"
#include "icegrids.h"

#define ANTENNA 1
#define ABDALATI 1

float gr37(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
int newfilt(ssmi *nmap, ssmi *smap);

/* int pole_fill(unsigned char *map, const int pole); */

int main(int argc, char *argv[])
{
  FILE *nin, *nout, *sin, *sout;
  ssmi nmap[NY_NORTH][NX_NORTH], smap[NY_SOUTH][NX_SOUTH];
  unsigned char nconc[NY_NORTH][NX_NORTH], sconc[NY_SOUTH][NX_SOUTH];
  unsigned char orig_nconc[NY_NORTH][NX_NORTH], orig_sconc[NY_SOUTH][NX_SOUTH];
  int i, j;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float g37[NY_NORTH][NX_NORTH];
  int debug;
  
  debug = (1 == 0);

  nin = fopen(argv[1], "r");
  sin = fopen(argv[2], "r");
  nout = fopen(argv[3], "w");
  sout = fopen(argv[4], "w");
  if (nin == NULL || sin == NULL) {
    printf("Failed to open an input file\n");
    return -1;
  }
  if (nout == NULL || sout == NULL) {
    printf("Failed to open an output file\n");
    return -1;
  }

  i = fread(&nmap[0][0], sizeof(ssmi), NX_NORTH*NY_NORTH, nin);
  if (i != NX_NORTH*NY_NORTH) {
    printf("Failed to read in full northern map!\n");
    return -2;
  }
  i = fread(&smap[0][0], sizeof(ssmi), NX_SOUTH*NY_SOUTH, sin);
  if (i != NX_SOUTH*NY_SOUTH) {
    printf("Failed to read in full southern map!\n");
    return -2;
  }
  getfld(&nmap[0][0], NX_NORTH*NY_NORTH, &orig_nconc[0][0], &g37[0][0], CONC_BAR);
  getfld(&smap[0][0], NX_SOUTH*NY_SOUTH, &orig_sconc[0][0], &g37[0][0], CONC_BAR);

  for (j = 0; j < NY_NORTH; j++) {
    for (i = 0; i < NX_NORTH; i++) {
       t19v = nmap[j][i].t19v / 100.;
       t19h = nmap[j][i].t19h / 100.;
       t22v = nmap[j][i].t22v / 100.;
       t37v = nmap[j][i].t37v / 100.;
       t37h = nmap[j][i].t37h / 100.;
       t85v = nmap[j][i].t85v / 100.;
       t85h = nmap[j][i].t85h / 100.;

       nmap[j][i].conc_bar = (int) (0.5 + 
            nasa_team(t19v, t19h, t22v, t37v, t37h, t85v, t85h, 'n', ANTENNA, ABDALATI) );
       if (nmap[j][i].conc_bar < (unsigned char) MIN_CONC) {
         nmap[j][i].conc_bar = 0;
       }

    }
  }

  for (j = 0; j < NY_SOUTH; j++) {
    for (i = 0; i < NX_SOUTH; i++) {
       t19v = smap[j][i].t19v / 100.;
       t19h = smap[j][i].t19h / 100.;
       t22v = smap[j][i].t22v / 100.;
       t37v = smap[j][i].t37v / 100.;
       t37h = smap[j][i].t37h / 100.;
       t85v = smap[j][i].t85v / 100.;
       t85h = smap[j][i].t85h / 100.;
       smap[j][i].conc_bar = (int) (0.5 + 
            nasa_team(t19v, t19h, t22v, t37v, t37h, t85v, t85h, 's', ANTENNA, ABDALATI) );
       if (smap[j][i].conc_bar < (unsigned char) MIN_CONC) {
         smap[j][i].conc_bar = 0;
       }
    }
  }

  if (debug) printf("Calling newfilt\n");
  newfilt(&nmap[0][0], &smap[0][0]);
  if (debug) printf("Returned from newfilt\n");

/* Fill in the polar gap */
  getfld(&nmap[0][0], NX_NORTH*NY_NORTH, &nconc[0][0], &g37[0][0], CONC_BAR);
  getfld(&smap[0][0], NX_SOUTH*NY_SOUTH, &sconc[0][0], &g37[0][0], CONC_BAR);

  pole_fill(&nconc[0][0], 1);
  pole_fill(&sconc[0][0], 2);

  fwrite(&nconc[0][0], sizeof(unsigned char), NX_NORTH*NY_NORTH, nout);
  fwrite(&sconc[0][0], sizeof(unsigned char), NX_SOUTH*NY_SOUTH, sout);

/* Look around at whether things have changed much, and where: */
  for (j = 0; j < NY_NORTH; j++) {
    for (i = 0; i < NX_NORTH; i++) {
      if (fabs(nconc[j][i] - orig_nconc[j][i]) > 5. 
           && nconc[j][i] != WEATHER
           && nconc[j][i] != NO_DATA
           && nconc[j][i] != BAD_DATA 
           && nconc[j][i] != LAND
           && orig_nconc[j][i] != WEATHER
           && orig_nconc[j][i] != NO_DATA
           && orig_nconc[j][i] != BAD_DATA 
           && orig_nconc[j][i] != LAND      
      ) {
        printf("large delta n %3d %3d  %3d %3d  %d\n",i,j,orig_nconc[j][i],
                    nconc[j][i], nconc[j][i] - orig_nconc[j][i]);
      }
    }
  }

  for (j = 0; j < NY_SOUTH; j++) {
    for (i = 0; i < NX_SOUTH; i++) {
      if (fabs(sconc[j][i] - orig_sconc[j][i]) > 5.  
           && sconc[j][i] != WEATHER
           && sconc[j][i] != NO_DATA
           && sconc[j][i] != BAD_DATA 
           && sconc[j][i] != LAND
           && orig_sconc[j][i] != WEATHER
           && orig_sconc[j][i] != NO_DATA
           && orig_sconc[j][i] != BAD_DATA 
           && orig_sconc[j][i] != LAND      
      ) {
        printf("large delta s %d %d  %d %d  %d\n",i,j,orig_sconc[j][i],
                    sconc[j][i], sconc[j][i] - orig_sconc[j][i]);
      }
    }
  }

  
  return 0;
}
/* Routine to fill in the unobserved polar gap */
/* Use a laplacean fill on the principle that the true field
   there is the smoothest (gradient sense) possible */
/* Robert Grumbine 4 June 1997 */

int pole_fill(unsigned char *map, const int pole)
{
    int i, j;
    float *a0, *a1, *a2, *delta;
    int nx, ny, polei, polej;
    int index, indexori, holex, holey;
    int iters, itmax = 15, jp, jm, ip, im;
    float delmax;

    if (dx != dy) {
      printf("Cannot use this polefill routine, it assumes that dx = dy.\n");
      printf(" dx = %f dy = %f\n",dx, dy);
      return -1;
    }

    if (pole == 1) {
      nx = NX_NORTH;
      ny = NY_NORTH;
      polei = polei_NORTH;
      polej = polej_NORTH;
    }
    else {
      nx = NX_SOUTH;
      ny = NY_SOUTH;
      polei = polei_SOUTH;
      polej = polej_SOUTH;
    }

    holex = 2*( 0.5 + (90. - MAX_LATITUDE) * 111.e3 / dx ) + 3;
    holey = holex;

    printf("holex, holey = %d %d \n",holex, holey);

    a0 = (float*) malloc(sizeof(float)*holex*holey);
    a1 = (float*) malloc(sizeof(float)*holex*holey);
    a2 = (float*) malloc(sizeof(float)*holex*holey);
    delta = (float*) malloc(sizeof(float)*holex*holey);

/* Make copy for local use */
    for (j = 0; j < holey; j++)
    { for (i = 0; i < holex; i++)
      {
        index = i+j*holex;
        indexori = (polei - holex/2 + i) + (polej - holey/2 + j)*nx;
        a0[index] = (float) map[indexori];
      }
    }

/* Fill in the 'no data' or 'bad data' points */
   for (j = 0; j < holey  ; j++)
   { for (i = 0; i < holex  ; i++)
     {
       index = i + j*holex;
       jp =  i      + (holey-1)*holex;
       jm =  i      +   0*holex;
       ip = holex-1 + j*holex;
       im =  0      + j*holex;
       if (a0[index] == NO_DATA || a0[index] == BAD_DATA || a0[index] == 0) {
         if ( a0[im] == NO_DATA || a0[ip] == NO_DATA ||
              a0[im] == BAD_DATA || a0[ip] == BAD_DATA ||
              a0[jm] == NO_DATA || a0[jp] == NO_DATA ||
              a0[jm] == BAD_DATA || a0[jp] == BAD_DATA ) {
           printf("Data problem with pole filling, no or bad data on bndy\n");
           return -1;
         }
         a1[index] = 0.5 *(a0[im] + (float)(i)/(float)(holex) *
                                     (a0[ip] - a0[im]) )
                   + 0.5 *(a0[jm] + (float)(j)/(float)(holey) *
                                     (a0[jp] - a0[jm]) );
       }
       else {
         a1[index] = a0[index];
       }
     }
   }

/* Now start some sort of an iteration scheme on the interior values */
   iters = 0;
   do {
     delmax = 0.;
     iters += 1;

     for (j = 1; j < holey-1; j++)
     { for ( i = 1; i < holex-1; i++)
       {
         index = i + j*holex;
         im =  i-1 + j*holex;
         ip =  i+1 + j*holex;
         jm =  i + (j-1)*holex;
         jp =  i + (j+1)*holex;

         delta[index] = -a1[index] + (a1[im] + a1[ip] + a1[jm] + a1[jp])/4.;
         a2[index] = a1[index]+delta[index];

         if (fabs(delta[index]) > delmax) delmax = fabs(delta[index]);
       }
     }
     printf("delmax = %f\n",delmax);
     for (j = 1; j < holey-1; j++)
     { for ( i = 1; i < holex-1; i++)
     {  index = i + j*holex;
        a1[index] = a2[index];
     }
     }

   }
   while ( fabs(delmax) > 0.5 && iters < itmax );

    for (j = 0; j < holey; j++)
    { for (i = 0; i < holex; i++)
      {
        index = i+j*holex;
        indexori = (polei - holex/2 + i) + (polej - holey/2 + j)*nx;
        if ( a0[index] == BAD_DATA || a0[index] == NO_DATA || a0[index] == 0) {
          map[indexori] = (int) (0.5 + a1[index]) ;
        }
      }
    }

   free(a0);
   free(a1);
   free(a2);
   free(delta);

   return 0;

}
int getfld(ssmi *ice, int npts, unsigned char *cfld, float *ffld, int sel)
{
/* Extract a desired field (specified by sel) from a full ssmi map
   (ice) and copy to both a character array (cfld) and a floating
   point array (ffdl) for some other routine to use.
   Tb Floats are scaled into degrees Kelvin, and have a 0.01 degree precision.
   Tb Chars are linearly rescaled according to o = (i-50)/2, where i is
     the floating number input, and o is the output, with 2 degree precision
     starting from 50 Kelvin.
   Ice concentrations are 1% precision, floating or character.
   Robert Grumbine 11 October 1994.
*/

  int i, limit;

  limit = npts;

  switch (sel) {
  case T19V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t19v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T19H :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t19h/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T22V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t22v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T37V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t37v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T37H :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t37h/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T85V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t85v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T85H :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t85h/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case CONC_BAR :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].conc_bar/100.;
      cfld[i] = (unsigned char) ice[i].conc_bar ;
    }
    break;
  case BAR_CONC :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].bar_conc/100.;
      cfld[i] = (unsigned char) ice[i].bar_conc ;
    }
    break;
  /* For new ssmi only -- Nov, 1998 */
  /*
  case COUNT :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].count;
      cfld[i] = (unsigned char) ice[i].count;
    }
    break;
  */
  default :
    return -1;
  }

  return 0;

}


void antenna(float *t19v, float *t19h, float *t22v, float *t37v, float *t37h,
             float *t85v, float *t85h);

void abdalati(float t19v, float t19h, float t22v, float t37v, float t37h,
              float t85v, float t85h,
              float *nt19v, float *nt19h, float *nt22v, float *nt37v, float *nt37h,
              float *nt85v, float *nt85h);

float nasa_team(const float t19v, const float t19h, const float t22v,
                const float t37v, const float t37h,
                const float t85v, const float t85h,
                const char pole, const int ant, const int regress)
{
/* Implementation of the NASA Team algorithm with 22 GHz weather
     filter.  C version by Robert Grumbine, based on Fortran code by
     M. Martino.  3/23/94.

C
C   THIS PROGRAM USES THE TEAM ALGORITHM TO CALCULATE THE TOTAL ICE
C   CONCENTRATION AND THE MULTIYEAR ICE CONCENTRATION.  INPUT ARE
C   19 VERT, 19 HORZ AND 37 VERT, 37 HORZ BRIGHTNESS TEMPERATURES.
C
C   COMPLETELY RE-WRITTEN FOR THE PC 16MAY91 M.G.MARTINO, STX CORP.
C   22V-19V GR WEATHER FILTER ADDED 22 JULY 93 BY MGM
C
C     GR FILTER IS .05
C    SSMI TIE POINTS  (1=19H, 4=37V)
C
C   Revised to use the Abdalati regreession coefficients for F-11,
C     with F-8 tie points.  95/06/22.
C   Revised to make antenna to brightness temperature correction 95/06/22.
*/
/* NORTH POLE TIE POINTS 08 MAR 91 F-8 */
      float t0n[7] = {100.8, 177.1, 00.0, 201.7, 0., 0., 0.};
      float tfn[7] = {242.8, 258.2, 00.0, 252.8, 0., 0., 0.};
      float tmn[7] = {203.9, 223.2, 00.0, 186.3, 0., 0., 0.};
/* North Pole tie points Jan 13, 1992 F-11*/
/*      float t0n[7] = { 99.8, 177.3, 00.0, 202.5, 0., 0., 0.};  */              
/*      float tfn[7] = {239.5, 249.7, 00.0, 244.2, 0., 0., 0.}; */
/*      float tmn[7] = {202.3, 221.3, 00.0, 184.6, 0., 0., 0.}; */

/* SOUTH POLE TIE POINTS 12 FEB 91 F-8 */
      float t0s[7] = {100.3, 176.6, 00.0, 200.5, 0., 0., 0.};
      float tfs[7] = {237.8, 249.8, 00.0, 243.3, 0., 0., 0.};
      float tms[7] = {193.7, 221.6, 00.0, 190.3, 0., 0., 0.};

/* South pole tie points 22 Nov 1994, approved 14 Oct 1994 F-11 */
/*      float t0s[7] = {100.9, 177.1, 00.0, 200.4, 0., 0., 0.}; */ /* open */
/*      float tfs[7] = {236.9, 249.0, 00.0, 242.8, 0., 0., 0.}; */ /* first */
/*      float tms[7] = {193.2, 221.3, 00.0, 190.3, 0., 0., 0.}; */ /* multi */

/* tie points within function */
      float *T0, *TF, *TM;
      float DW[2], DF[2], DM[2];
      float SW[2], SF[2], SM[2];

/* Local variables */
      float gr37, gr22, polar;
      float nf19, nm19, dd19, fy, my, total;
      float a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1 ;
      float nt19v, nt19h, nt22v, nt37v, nt37h, nt85v, nt85h;

/* Preliminary check on whether the data are useable */
       if ( (t22v + t19v) == 0. || (t19v + t19h) == 0. || (t37v + t19v)==0. )
       {
         return (float) BAD_DATA;
       }

/*
C   CALCULATE PARAMETERS FOR ICE CONCENTRATION ALGORITHM
*/

      if (pole == 'n') {
        T0 = &t0n[0];
        TF = &tfn[0];
        TM = &tmn[0];
      }
      else if (pole == 's') {
        T0 = &t0s[0];
        TF = &tfs[0];
        TM = &tms[0];
      }
      else {
        printf("specified a pole that doesn't exist!!\n");
        return -1.;
      }

       DW[0]=T0[1]-T0[0];
       DF[0]=TF[1]-TF[0];
       DM[0]=TM[1]-TM[0];

       SW[0]=T0[1]+T0[0];
       SF[0]=TF[1]+TF[0];
       SM[0]=TM[1]+TM[0];

       DW[1]=T0[3]-T0[1];
       DF[1]=TF[3]-TF[1];
       DM[1]=TM[3]-TM[1];

       SW[1]=T0[3]+T0[1];
       SF[1]=TF[3]+TF[1];
       SM[1]=TM[3]+TM[1];

       a1=DM[0]*DW[1]-DM[1]*DW[0];
       b1=DM[1]*SW[0]-DW[1]*SM[0];
       c1=DW[0]*SM[1]-DM[0]*SW[1];
       d1=SM[0]*SW[1]-SM[1]*SW[0];
/*       d1=SM[0]*SW[0]-SM[1]*SW[0]; */

       i1=DF[1]*DW[0]-DF[0]*DW[1];
       j1=DW[1]*SF[0]-DF[1]*SW[0];
       k1=SW[1]*DF[0]-DW[0]*SF[1];
       l1=SF[1]*SW[0]-SF[0]*SW[1];

       e1=DF[0]*(DM[1]-DW[1])+DW[0]*(DF[1]-DM[1])+DM[0]*(DW[1]-DF[1]);
       f1=DF[1]*(SM[0]-SW[0])+DW[1]*(SF[0]-SM[0])+DM[1]*(SW[0]-SF[0]);
       g1=DF[0]*(SW[1]-SM[1])+DW[0]*(SM[1]-SF[1])+DM[0]*(SF[1]-SW[1]);
/*       g1=DF[0]*(SW[0]-SM[1])+DW[0]*(SM[1]-SF[1])+DM[0]*(SF[1]-SW[1]); */
       h1=SF[1]*(SW[0]-SM[0])+SW[1]*(SM[0]-SF[0])+SM[1]*(SF[0]-SW[0]);

/* Recompute the brightness temperatures, if needed, via the Abdalati,
   1995 calibration of F-8 versus F-11 */
       if (regress == 1) {
         abdalati(t19v, t19h, t22v, t37v, t37h, t85v, t85h,
                  &nt19v, &nt19h, &nt22v, &nt37v, &nt37h, &nt85v, &nt85h);
       }

/* Correct from antenna temperatures to brightness temperatures, if
   needed, following Katz codes of 3/6/95 */
       if (ant == 1) {
         antenna(&nt19v, &nt19h, &nt22v, &nt37v, &nt37h, &nt85v, &nt85h);
       }

/* Now, finally, compute the ice concentration */
       gr22  =  (nt22v - nt19v) / (nt22v + nt19v);
       polar =  (nt19v - nt19h) / (nt19v + nt19h);
       gr37  =  (nt37v - nt19v) / (nt37v + nt19v);

       total = (float) NO_DATA;
       if ( (gr37 <= (float) GR37LIM) && (gr22 <= (float) GR22LIM) ) {
           nf19=a1+b1*polar+c1*gr37+d1*polar*gr37;
           nm19=i1+j1*polar+k1*gr37+l1*polar*gr37;
           dd19=e1+f1*polar+g1*gr37+h1*polar*gr37;
           if (dd19 == 0.) {  printf("zero divisor for concentrations\n");
                              return (float) BAD_DATA; }
           fy = nf19/dd19;
           my = nm19/dd19;
           total = (my + fy)*100.;
           if (total < -20.0 ) return (float) BAD_DATA;
           if (total <   0.0 ) total = 0.;
       }
       else {
        total = (float) WEATHER; /* Set weather filtered points to weather flag */
       }

      return total;
}
void antenna(float *TB19V, float *TB19H, float *TB22V,
             float *TB37V, float *TB37H, float *TB85V, float *TB85H)
{
      float YES19V, YES19H, YES22V, YES37V, YES37H, YES85V, YES85H;

      float AP19V = 0.969, AP19H = .969, AP22V = .974;
      float AP37V = .986, AP37H = .986,  AP85V = .988, AP85H = .988;
      float BP19V = .00473, BP19H = .00415, BP22V = .0107;
      float BP37V = .0217, BP37H = .02612, BP85V = .01383, BP85H = .01947;

      float C19V, C19H, C22V, C37V, C37H, C85V, C85H;
      float D19V, D19H, D22V, D37V, D37H, D85V, D85H;

      C19V = 1/(AP19V*(1-BP19V));
      C19H = 1/(AP19H*(1-BP19H));
      C22V = 1/(AP22V*(1-BP22V));
      C37V = 1/(AP37V*(1-BP37V));
      C37H = 1/(AP37H*(1-BP37H));
      C85V = 1/(AP85V*(1-BP85V));
      C85H = 1/(AP85H*(1-BP85H));
      D19V = C19V*BP19V;
      D19H = C19H*BP19H;
      D22V = C22V*BP22V;
      D37V = C37V*BP37V;
      D37H = C37H*BP37H;
      D85V = C85V*BP85V;
      D85H = C85H*BP85H;

        YES19V = C19V * (*TB19V) - D19V * (*TB19H);
        YES19H = C19H * (*TB19H) - D19H * (*TB19V);
        YES22V = C22V * (*TB22V) - D22V * (0.653 * (*TB19H) + 96.6);
        YES37V = C37V * (*TB37V) - D37V * (*TB37H);
        YES37H = C37H * (*TB37H) - D37H * (*TB37V);
        YES85V = C85V * (*TB85V) - D85V * (*TB85H);
        YES85H = C85H * (*TB85H) - D85H * (*TB85V);

     *TB19V = YES19V;
     *TB19H = YES19H;
     *TB22V = YES22V;
     *TB37V = YES37V;
     *TB37H = YES37H;
     *TB85V = YES85V;
     *TB85H = YES85H;

     return;
}
void abdalati(float t19v, float t19h, float t22v, float t37v, float t37h,
              float t85v, float t85h,
              float *nt19v, float *nt19h, float *nt22v, float *nt37v, float *nt37h,
              float *nt85v, float *nt85h)
{
/* Recompute the brightness temperatures via the Abdalati, 1995 calibration
   of F-8 versus F-11 */
       *nt19h = 1.013 * t19h + (-1.89);
       *nt19v = 1.013 * t19v + (-2.51);
       *nt22v = 1.014 * t22v + (-2.73);
       *nt37h = 1.024 * t37h + (-4.22);
       *nt37v = 1.000 * t37v + ( 0.052);
       *nt85v = t85v;
       *nt85h = t85h;

       return;
}


int newfilt(ssmi *nmap, ssmi *smap)
{
  int i, j;
  float g37[NY_NORTH][NX_NORTH];
  int debug;
  unsigned char nconc[NY_NORTH][NX_NORTH], sconc[NY_SOUTH][NX_SOUTH];

  debug = (0 == 1);

  getfld(nmap, NX_NORTH*NY_NORTH, &nconc[0][0], &g37[0][0], BAR_CONC);
  getfld(smap, NX_SOUTH*NY_SOUTH, &sconc[0][0], &g37[0][0], BAR_CONC);

/* Find the northern hemisphere gradient ratio */
  for (j = 0; j < NY_NORTH  ; j++) {
    for (i = 0; i < NX_NORTH  ; i++) {
      g37[j][i] = gr37(nmap, i, j, NX_NORTH, NY_NORTH, 0);
    }
  }
/* Loop over all points.  If, in any case, the 2 pt averaged gradient ratio
     for 37-19 v is greater than the cut off, then filter out the ice
     concentration */

  for (j = 1; j < NY_NORTH - 1 ; j++) {
    for (i = 1; i < NX_NORTH - 1 ; i++) {
      if (nconc[j][i] != 0 && nconc[j][i] != BAD_DATA) {

        if (nconc[j+1][i] != BAD_DATA) {
          if (g37[j+1][i] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j-1][i] != BAD_DATA) {
          if (g37[j-1][i] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j][i-1] != BAD_DATA) {
          if (g37[j][i-1] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j][i+1] != BAD_DATA) {
          if (g37[j][i+1] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }

      } /* End of filtration testing */

    }
  }

  for (j = 0; j < NY_NORTH; j++) {
    for (i = 0; i < NX_NORTH; i++) {
       nmap[j*NX_NORTH +i].bar_conc = nconc[j][i];
    }
  }

/* Need to put southern filtering in here */
  for (j = 0; j < NY_SOUTH  ; j++) {
    for (i = 0; i < NX_SOUTH  ; i++) {
      g37[j][i] = gr37(smap, i, j, NX_SOUTH, NY_SOUTH, 0);
    }
  }
/* Note above that we've used the same array for both north and south
   gradients */
  for (j = 1; j < NY_SOUTH - 1; j++) {
    for (i = 1; i < NX_SOUTH - 1; i++) {
      if (sconc[j][i] != 0 && sconc[j][i] != BAD_DATA) {

        if (sconc[j+1][i] != BAD_DATA) {
          if (g37[j+1][i] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j-1][i] != BAD_DATA) {
          if (g37[j-1][i] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j][i-1] != BAD_DATA) {
          if (g37[j][i-1] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j][i+1] != BAD_DATA) {
          if (g37[j][i+1] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }

      } /* End of filtration testing */

    }
  }

  for (j = 0; j < NY_SOUTH; j++) {
    for (i = 0; i < NX_SOUTH; i++) {
       smap[j*NX_SOUTH + i].bar_conc = sconc[j][i];
    }
  }

  return 0;
}

float gr37(const ssmi *map, const int i, const int j,
                            const int nx, const int ny, const int range)
{
   int index, ti, tj, count;
   float t19v, t37v, tempor;

   t19v = 0.0;
   t37v = 0.0;
   count = 0;

   if (range != 0) {
     for (tj = j-range ; tj < j+range ; tj++) {
       for (ti = i - range ; ti < i+range; ti++) {
         index = ti + tj*nx;
         if (index < 0 || index >= nx*ny) continue;
         if (map[index].t19v != 0 && map[index].t37v != 0) {
           count += 1;
           t19v += map[index].t19v;
           t37v += map[index].t37v;
         }
       }
     }
 
     t37v = t37v / count;
     t19v = t19v / count;
   }
   else {
     index = i + j*nx;
     t19v = map[index].t19v;
     t37v = map[index].t37v;
   }

   if (t19v != 0.0 && t37v != 0.0 ) {
     tempor = (t37v - t19v) / (t37v + t19v);
   }
   else {tempor = 0.0;}

   return tempor;
}

