#include "grid_math.h"
#include "icessmi.h"

// Elements for the TEAM2 algorithm 
#include "team2.C"
// Done with elements for the team2 algorithm


void test(team2_tables &tab) ;

/* For the old algorithm */
int bad_low = 0;
int bad_high = 0;
int crop_low = 0;
int filt37   = 0;
int filt22   = 0;

int main(void) {
  team2_tables arctic, antarctic;

  arctic.tbmfy.resize(n_atm, n_tb);
  arctic.tbmow.resize(n_atm, n_tb);
  arctic.tbmcc.resize(n_atm, n_tb);
  arctic.tbmthin.resize(n_atm, n_tb);
  arctic.pole = 'n';
  arctic_tables(arctic);
  //lookuptable(arctic.tbmow, arctic.tbmfy, arctic.tbmcc, arctic.tbmthin, arctic.phi19, arctic.phi85);
  lookuptable(arctic);


  antarctic.tbmfy.resize(n_atm, n_tb);
  antarctic.tbmow.resize(n_atm, n_tb);
  antarctic.tbmcc.resize(n_atm, n_tb);
  antarctic.tbmthin.resize(n_atm, n_tb);
  antarctic.pole = 's';
  antarctic_tables(antarctic);
  //lookuptable(antarctic);


  test(arctic);

  return 0;
}
void test(team2_tables &tab) {
  float v19, h19, v22, v37, h37, v85, h85;
  float x, y, z;
  float sum = 0, sumsq = 0;
  int count = 0;
  //for (v19 = 75; v19 < 300; v19 += 40) {
  //for (h19 = 75; h19 < 300; h19 += 40) {
  //for (v22 = 75; v22 < 300; v22 += 40) {
  //for (v37 = 75; v37 < 300; v37 += 40) {
  //for (h37 = 75; h37 < 300; h37 += 40) {
  //for (v85 = 75; v85 < 300; v85 += 40) {
  //for (h85 = 75; h85 < 300; h85 += 40) {
  //   x = nt2(v19, h19, v22, v37, h37, v85, h85,
  //           LUT19, LUT19thin, LUT85, LUT85thin, LUTDGR, LUTGR37,
  //           "n", phi19, phi85);
  //   y = nasa_team(v19, h19, v22, v37, h37, v85, h85, pole, 1, 13);
  //   z = nasa_team(v19, h19, v22, v37, h37, v85, h85, pole, 0, 13);
  //   if (x < 130 && y < 130) {
  //     if (y > 100 && y < 130) y = 100;
  //     if (z > 100 && z < 130) z = 100;
  //     printf("%5.1f %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f  %f %f %f\n",
  //              v19, h19, v22, v37, h37, v85, h85, x, y, z);
  //     sum += y - x;
  //     sumsq += (y-x)*(y-x);
  //     count += 1;
  //   }
  //}
  //}
  //}
  //}
  //}
  //}
  //}
  //printf("avg, avg square, rms %f %f %f\n",sum/count, sumsq/count, sqrt(sumsq/count) );

  #define NX 770
  #define NY 930
  grid2<ssmi> north(NX,NY);
  mvector<float> t19v(NX*NY);
  mvector<float> t19h(NX*NY);
  mvector<float> t22v(NX*NY);
  mvector<float> t37v(NX*NY);
  mvector<float> t37h(NX*NY);
  mvector<float> t85v(NX*NY);
  mvector<float> t85h(NX*NY);
  mvector<float> old(NX*NY);
  mvector<float> team2(NX*NY);
  FILE *fin;
  ijpt loc;

  fin = fopen("n3ssmi.20050922","r");
  north.binin(fin);
  fclose(fin);
  count = 0;
  sum = 0;
  sumsq = 0;
  for (loc.j = 0; loc.j < NY; loc.j++) {
  for (loc.i = 0; loc.i < NX; loc.i++) {
    if (north[loc].bar_conc < 130) {
      old[count] = min((int)100, (int)north[loc].conc_bar) ;
      t19v[count] = north[loc].t19v / 100.;
      t19h[count] = north[loc].t19h / 100.;
      t22v[count] = north[loc].t22v / 100.;
      t37v[count] = north[loc].t37v / 100.;
      t37h[count] = north[loc].t37h / 100.;
      t85v[count] = north[loc].t85v / 100.;
      t85h[count] = north[loc].t85h / 100.;

      team2[count] = nt2(t19v[count], t19h[count], t22v[count], t37v[count], 
                         t37h[count], t85v[count], t85h[count], tab );
      if (team2[count] < 200) {
        printf("%6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %5.1f %5.1f\n",
               t19v[count], t19h[count], 
               t22v[count], t37v[count], t37h[count], t85v[count], t85h[count],
               old[count], team2[count] );
  
        sum += (team2[count] - old[count]);
        sumsq += (team2[count] - old[count])*(team2[count] - old[count]);
        count += 1;
      }
    }
  }
  }
  count -= 1;
  printf("found %d ice points on grid\n",count);
  printf("avg, avg square, rms %f %f %f\n",sum/count, sumsq/count, sqrt(sumsq/count) );

  return;

}
