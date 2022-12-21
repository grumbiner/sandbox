#include "amsrice.h"

/* Average (as needed) the brightness temperatures and ice concentrations
     on the sea ice grid. */
/* Compute the ice concentration with the averaged ice temperatures (a1 
     files) */
/* Robert Grumbine 10 February 1995 */
/* Apply the minimum concentration cutoff  Robert Grumbine 8 June 2001 */

int ice_avg_data(amsr_tmp *north_tmp, amsr_tmp *south_tmp,
                 amsr *north, amsr *south, 
                 const int north_pts, const int south_pts, 
                 amsr_team2_tables &arctic, amsr_team2_tables &antarctic) { 
  int i, nlim, slim;

  nlim = north_pts;
  slim = south_pts;
  
  for (i = 0; i < nlim ; i++) {
    if (north_tmp[i].count == 0) {
      /* Do it this way to preserve the weather flags we may have set */
      north[i].conc_bar = north_tmp[i].conc_bar;
      north[i].bar_conc = north_tmp[i].conc_bar;
      north[i].hires_conc = north_tmp[i].conc_bar;
      north[i].t19v = 0;
      north[i].t19h = 0;
      north[i].t24v = 0;
      north[i].t24h = 0;
      north[i].t37v = 0;
      north[i].t37h = 0;
      north[i].t89v = 0;
      north[i].t89h = 0;
      north[i].count = 0;
      north[i].weather_count = north_tmp[i].weather_count;
    }
    else {
      north[i].t19v = (int) (0.5 + (float) north_tmp[i].t19v / \
                                        (float) north_tmp[i].count);
      north[i].t19h = (int) (0.5 + (float) north_tmp[i].t19h / \
                                        (float) north_tmp[i].count);
      north[i].t24v = (int) (0.5 + (float) north_tmp[i].t24v / \
                                        (float) north_tmp[i].count);
      north[i].t24h = (int) (0.5 + (float) north_tmp[i].t24h / \
                                        (float) north_tmp[i].count);
      north[i].t37v = (int) (0.5 + (float) north_tmp[i].t37v / \
                                        (float) north_tmp[i].count);
      north[i].t37h = (int) (0.5 + (float) north_tmp[i].t37h / \
                                        (float) north_tmp[i].count);
      north[i].t89v = (int) (0.5 + (float) north_tmp[i].t89v / \
                                        (float) north_tmp[i].count);
      north[i].t89h = (int) (0.5 + (float) north_tmp[i].t89h / \
                                        (float) north_tmp[i].count);
      north[i].conc_bar = (int) (0.5 + (float) north_tmp[i].conc_bar / 
                                       (float) north_tmp[i].count);
      north[i].count    = north_tmp[i].count;
      north[i].weather_count = north_tmp[i].weather_count;

        north[i].bar_conc = (unsigned char) (0.5 + nasa_team2(
          (float)north[i].t19v / 10.,
          (float)north[i].t19h / 10.,
          (float)north[i].t24v / 10.,
          (float)north[i].t37v / 10.,
          (float)north[i].t37h / 10.,
          (float)north[i].t89v / 10.,
          (float)north[i].t89h / 10.,
          arctic) );
        // need to test on locale

      #ifdef VERBOSE2
        printf("averaged data, %4d, %5d %5d %5d %5d %3d %3d %5d %3d\n",
        i, north[i].t19v, north[i].t19h, north[i].t24v, north[i].t37v,
        north[i].conc_bar, north[i].bar_conc, 
        north[i].conc_bar - north[i].bar_conc, north[i].count);
      #endif 
      
    }
  }

  for (i = 0; i < slim ; i++) {
    if (south_tmp[i].count == 0) {
      south[i].conc_bar = south_tmp[i].conc_bar;
      south[i].bar_conc = south_tmp[i].conc_bar;
      south[i].hires_conc = south_tmp[i].conc_bar;
      south[i].t19v = 0;
      south[i].t19h = 0;
      south[i].t24v = 0;
      south[i].t24h = 0;
      south[i].t37v = 0;
      south[i].t37h = 0;
      south[i].t89v = 0;
      south[i].t89h = 0;
      south[i].count = 0;
      south[i].weather_count = south_tmp[i].weather_count;
    }
    else {
      south[i].t19v = (int) (0.5 + (float) south_tmp[i].t19v / 
                                        (float) south_tmp[i].count);
      south[i].t19h = (int) (0.5 + (float) south_tmp[i].t19h /
                                        (float)  south_tmp[i].count);
      south[i].t24v = (int) (0.5 + (float) south_tmp[i].t24v / 
                                        (float) south_tmp[i].count);
      south[i].t24h = (int) (0.5 + (float) south_tmp[i].t24h / 
                                        (float) south_tmp[i].count);
      south[i].t37v = (int) (0.5 + (float) south_tmp[i].t37v / 
                                        (float) south_tmp[i].count);
      south[i].t37h = (int) (0.5 + (float) south_tmp[i].t37h / 
                                        (float) south_tmp[i].count);
      south[i].t89v = (int) (0.5 + (float) south_tmp[i].t89v / 
                                        (float) south_tmp[i].count);
      south[i].t89h = (int) (0.5 + (float) south_tmp[i].t89h / 
                                        (float) south_tmp[i].count);
      south[i].conc_bar = (int) (0.5 + (float)south_tmp[i].conc_bar / 
                                       (float)south_tmp[i].count);
      south[i].count    = south_tmp[i].count;
      south[i].weather_count = south_tmp[i].weather_count;

      //printf("s - team2\n");
      south[i].bar_conc = (unsigned char) (0.5 + nasa_team2(
        (float)south[i].t19v / 10.,
        (float)south[i].t19h / 10.,
        (float)south[i].t24v / 10.,
        (float)south[i].t37v / 10.,
        (float)south[i].t37h / 10.,
        (float)south[i].t89v / 10.,
        (float)south[i].t89h / 10.,
        antarctic) );

    }
  }

/* Minimum concentration cutoffs  8 June 2001 */
  for (i = 0; i < nlim; i++) {
     if (north[i].conc_bar < MIN_CONC) north[i].conc_bar = 0;
     if (north[i].bar_conc < MIN_CONC) north[i].bar_conc = 0;
     if (north[i].hires_conc < MIN_CONC) north[i].hires_conc = 0;
  }
  for (i = 0; i < slim; i++) {
     if (south[i].conc_bar < MIN_CONC) south[i].conc_bar = 0;
     if (south[i].bar_conc < MIN_CONC) south[i].bar_conc = 0;
     if (south[i].hires_conc < MIN_CONC) south[i].hires_conc = 0;
  }

/* Weather filter based on counts of good vs bad data on point  30 April 2004 */
/* By way of above computation, bar_conc can be weather even if conc_bar is not */
  for (i = 0; i < nlim; i++) {
     if (north[i].bar_conc == WEATHER) {
       north[i].conc_bar = WEATHER;
       north[i].bar_conc = WEATHER;
       north[i].hires_conc = WEATHER;
     }
     if (north[i].weather_count >= north[i].count / 2 && 
         north[i].bar_conc <= MAX_CONC ) {
       north[i].conc_bar = WEATHER;
       north[i].bar_conc = WEATHER;
       north[i].hires_conc = WEATHER;
     }      
  }
  for (i = 0; i < slim; i++) {
     if (south[i].bar_conc == WEATHER) {
       south[i].conc_bar = WEATHER;
       south[i].bar_conc = WEATHER;
       south[i].hires_conc = WEATHER;
     }
     if (south[i].weather_count >= south[i].count / 2 && 
         south[i].bar_conc <= MAX_CONC ) {
       south[i].conc_bar = WEATHER;
       south[i].bar_conc = WEATHER;
       south[i].hires_conc = WEATHER;
     }      
  }

  #ifdef VERBOSE
    printf("About to leave ice_avg_data\n");
    fflush(stdout);
  #endif

  return 0;

}

/* Apply a land mask to the concentrations (but not the brightness 
     temperatures ) */
/* Robert Grumbine 28 March 1995 */

int ice_mask( amsr *north, amsr *south, 
              const int north_pts, const int south_pts, 
              unsigned char *nmap, unsigned char *smap    ) { 
  int i, nlim, slim;

  nlim = north_pts;
  slim = south_pts;
  
  for (i = 0; i < nlim ; i++) {
    if ( ((int) nmap[i]) == ((int) LAND) ) {
      north[i].conc_bar = LAND ;
      north[i].bar_conc = LAND ;
      north[i].hires_conc = LAND ;
    }
    if ( ((int) nmap[i]) == ((int) COAST) ) {
      north[i].conc_bar = COAST ;
      north[i].bar_conc = COAST ;
      north[i].hires_conc = COAST ;
    }
  }

  for (i = 0; i < slim ; i++) {
    if (smap[i] == LAND) {
      south[i].conc_bar = LAND ;
      south[i].bar_conc = LAND ;
      south[i].hires_conc = LAND ;
    }
    if ( ((int) smap[i]) == ((int) COAST) ) {
      south[i].conc_bar = COAST ;
      south[i].bar_conc = COAST ;
      south[i].hires_conc = COAST ;
    }
  }

  return 0;

}

/* Zero out the information in the temporary data files for amsr 
      average computation */
/* Robert Grumbine 15 December 1994 */

int ice_zero(amsr_tmp *north_tmp, amsr_tmp *south_tmp, 
              const int north_pts, const int south_pts) {
  int i;
  amsr_tmp blank;

  blank.t19v = 0;
  blank.t19h = 0;
  blank.t24v = 0;
  blank.t24h = 0;
  blank.t37v = 0;
  blank.t37h = 0;
  blank.t89v = 0;
  blank.t89h = 0;
  blank.conc_bar  = NO_DATA;
  blank.hires_bar = NO_DATA;
  blank.count     = 0;
  blank.weather_count    = 0;

  for (i = 0; i < north_pts; i++) { 
    north_tmp[i] = blank;
  }

  for (i = 0; i < south_pts; i++) { 
    south_tmp[i] = blank;
  }

  return 0;

}
