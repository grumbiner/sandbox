#include <cstdio>
#include <cstdlib>
#include <cmath> 

#include "amsrice.h"
#include "icegrids.h"
#include "grid_math.h"

#include "amsr_team2.C"

/* Main program for decoding the amsr orbital records. */
/* Compute sea ice concentrations -- nasa_team2 algorithm */


/* ************************************************ */
/* The following are for data qc and error tracking */
/* for process_bufr */
int err_19h_range = 0;
int err_19v_range = 0;
int err_24v_range = 0;
int err_24h_range = 0;
int err_37v_range = 0;
int err_37h_range = 0;
int err_89v_range = 0;
int err_89h_range = 0;
int err_19_polar = 0;
int err_37_polar = 0;
int err_89_polar = 0;
int err_lat      = 0;
int err_lon      = 0;
/* For the algorithm */
int bad_low = 0;
int bad_high = 0;
int crop_low = 0;
int filt37   = 0;
int filt24   = 0;
/* For filtration */
int efilt_37_n = 0;
int efilt_37_s = 0;
int efilt_24_n = 0;
int efilt_24_s = 0;

// Generic file for conducting other training/testing:
FILE *tester;

/* ************************************************ */
float fmax(float x, float y);
float fmax(float x, float y) {
  if (x > y) {
    return x;
  }
  return y;
}
extern float hires(amsr *map, int nx, int ny, int polei, int polej, 
                      unsigned char *mask) ;

/* ************************************************ */
int main(int argc, char *argv[]) {
  scan_points  a_scan_points;
  FILE *fin, *outn, *outs, *nland, *sland;
  FILE *nraw, *sraw;
  char fname[80];

  amsr *south, *north;
  amsr_tmp *north_tmp, *south_tmp;
  float  rfld_n[NY_NORTH][NX_NORTH];
  float  rfld_s[NY_SOUTH][NX_SOUTH];
  unsigned char  fld_n[NY_NORTH][NX_NORTH];
  unsigned char  fld_s[NY_SOUTH][NX_SOUTH];

  int north_pts, south_pts;
  int i, j, nerrs, nread;
  int lines_used = 0, err_spots = 0;
// For team2
  team2_tables arctic, antarctic;

  printf("entered the AMSR ice concentration program\n"); fflush(stdout);
                                                                                
// Team2 initialization:
  arctic.tbmfy.resize(n_atm, n_tb);
  arctic.tbmow.resize(n_atm, n_tb);
  arctic.tbmcc.resize(n_atm, n_tb);
  arctic.tbmthin.resize(n_atm, n_tb);
  arctic.pole = 'n';
  arctic_tables(arctic);
  lookuptable(arctic);

  antarctic.tbmfy.resize(n_atm, n_tb);
  antarctic.tbmow.resize(n_atm, n_tb);
  antarctic.tbmcc.resize(n_atm, n_tb);
  antarctic.tbmthin.resize(n_atm, n_tb);
  antarctic.pole = 's';
  antarctic_tables(antarctic);
  lookuptable(antarctic);
// End Team2 initialization

  printf("have read in the tables \n"); fflush(stdout);


/* allocate the amsr arrays */
  north_tmp = new amsr_tmp[NX_NORTH*NY_NORTH];
  south_tmp = new amsr_tmp[NX_SOUTH*NY_SOUTH];
  north     = new amsr[NX_NORTH*NY_NORTH];
  south     = new amsr[NX_SOUTH*NY_SOUTH];
  if (north == (amsr*) NULL || south == (amsr*) NULL || 
      south_tmp == (amsr_tmp*) NULL || north_tmp == (amsr_tmp*) NULL) {
    printf("Failed to new one of the amsr fields!\n");
    fflush(stdout);
    return 1;
  }
/* Set internal constants and initialize arrays */
  north_pts = (NX_NORTH)*(NY_NORTH);
  south_pts = (NX_SOUTH)*(NY_SOUTH);
  ice_zero(north_tmp, south_tmp, north_pts, south_pts);

 
  printf("Preparing to open data files\n"); fflush(stdout);

/* Open data files */
  fin = fopen(argv[1], "r");
  nland = fopen(argv[2], "r");
  sland = fopen(argv[3], "r");
  outn  = fopen(argv[4], "w");
  outs  = fopen(argv[5], "w");
  nraw  = fopen(argv[6], "w");
  sraw  = fopen(argv[7], "w");
  if (fin == (FILE *) NULL   || 
      nland == (FILE *) NULL || sland == (FILE *) NULL ) {
    printf("Failed to open one of the input files!\n");
    printf("Will output fields of 'no data'\n");
    if (fin == (FILE *) NULL ) {printf(" - input file %s\n",argv[1]);}
    if (nland == (FILE *) NULL ) {printf(" - north land in %s\n", argv[2]);}
    if (sland == (FILE *) NULL ) {printf(" - south land in %s\n", argv[3]);}
  }    
  if ( outn == (FILE *) NULL ||  outs == (FILE *) NULL || 
       nraw == (FILE *) NULL ||  sraw == (FILE *) NULL) {
    printf("Failed to open one of the output files!\n");
    printf("Cannot repair!!!!!!!!\n");
    if (outn == (FILE *) NULL ) {printf(" - north out %s\n", argv[4]);}
    if (outs == (FILE *) NULL ) {printf(" - south out %s\n", argv[5]);}
    if (nraw == (FILE *) NULL ) {printf(" - north raw out %s\n", argv[6]);}
    if (sraw == (FILE *) NULL ) {printf(" - south raw out %s\n", argv[7]);}
    return 2;
  }

  #ifdef TESTER
    tester = fopen("tester","w");
  #endif

// Start initializing data fields
/* Read in the land masks to fld_n, fld_s */
  if ((nland != (FILE *) NULL) && (sland != (FILE *) NULL) ) {
    fread(fld_n, sizeof(unsigned char), north_pts, nland);
    fread(fld_s, sizeof(unsigned char), south_pts, sland);
  }
  printf("Have read in land masks, now trying to set non-land to nodata\n"); fflush(stdout);

  ijpt loc;
  for (loc.j = 0; loc.j < NY_NORTH; loc.j++) {
  for (loc.i = 0; loc.i < NX_NORTH; loc.i++) {
    if (fld_n[loc.j][loc.i] == 0) fld_n[loc.j][loc.i] = NO_DATA;
  }
  }
  for (loc.j = 0; loc.j < NY_SOUTH; loc.j++) {
  for (loc.i = 0; loc.i < NX_SOUTH; loc.i++) {
    if (fld_s[loc.j][loc.i] == 0) fld_s[loc.j][loc.i] = NO_DATA;
  }
  }


  printf("AMSR processing currently disabled as no data are arriving, 30 March 2012\n");
  printf("  Writing out fields of 'undefined'\n");
  fflush(stdout);
  fwrite(fld_n, sizeof(unsigned char), north_pts, nraw);
  fwrite(fld_s, sizeof(unsigned char), south_pts, sraw);
  fwrite(north, sizeof(amsr), north_pts, outn);
  fwrite(south, sizeof(amsr), south_pts, outs);
  return 0;

/* Begin stripping out data */
  while (!feof(fin) && (fin != (FILE *) NULL) ) {

    j = 0;
    while (!feof(fin) ) {  
      j++;
      nread = fread(&a_scan_points, sizeof(scan_points), 1, fin);
      
      nerrs = process_bufr(&a_scan_points);
      lines_used += 1;
      err_spots += nerrs;
      #ifdef VERBOSE
      if (nerrs != 0) {
        printf("%4d errors in record %6d \n",nerrs, j);
        fflush(stdout);
      }
      #endif

      if (nread != nerrs) {
        ice_add_bufr(north_tmp, south_tmp, &a_scan_points, nread, arctic, antarctic); 
      }

    }
  
  } /* End while - reading file */
  fclose(fin);
  printf("finished reading the data file\n"); fflush(stdout);
  
  /* Compute averaged tb and concentrations */
  ice_avg_data(north_tmp, south_tmp, north, south, north_pts, south_pts, arctic, antarctic);

/* Apply newer filtering techniques 10/29/95 BG */
/* Revise for hires grids 16 March 2004 */
/* Drop in moving to AMSR   -- newfilt(north, south);  */

  getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], BAR_CONC);
  getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], BAR_CONC);

/* Fill in pole for output purposes */
  pole_fill(&fld_n[0][0], 1);
  pole_fill(&fld_s[0][0], 2);
  fwrite(fld_n, sizeof(unsigned char), north_pts, nraw);
  fwrite(fld_s, sizeof(unsigned char), south_pts, sraw);

/* Mask out the land now */
  ice_mask(north, south, north_pts, south_pts, &fld_n[0][0], &fld_s[0][0]);

/* Write out full amsr and concentration data files */
  fwrite(north, sizeof(amsr), north_pts, outn);
  fwrite(south, sizeof(amsr), south_pts, outs);
  fclose(outn);
  fclose(outs);

/* Expt: Write out the error counts by type */
  if (lines_used > 0) {
    printf("Total lores spots: %8d  Error spots %8d fraction %6.4f\n",
            lines_used*NSPOTS, err_spots, 
            (float) err_spots / (float) (NSPOTS*lines_used) );
    printf("Error stats\n");
    printf("  lat %d lon %d\n",err_lat, err_lon);
    printf("  range 19v %5d 19h %5d 24v %5d 24h %5d 37v %5d 37h %5d 89v %5d 89h %5d\n",
      err_19v_range, err_19h_range, err_24v_range, err_24h_range,
      err_37v_range, err_37h_range, err_89v_range, err_89h_range);
    printf("  polar 19GHz %5d 37GHz %5d 89GHz %6d\n",
      err_19_polar, err_37_polar, err_89_polar);
    printf("  bad_low %4d crop_low %4d bad high %4d\n",bad_low, crop_low, bad_high);
    printf("  filt37 %d filt24 %d efilt_37_n %6d efilt_37_s %6d\n",
      filt37, filt24, efilt_37_n, efilt_37_s);
  }
  else {
    printf("Did not find data for satellite\n");
  }

/* Expt: Write out the data count maps */
  #ifdef VERBOSE
    outn = fopen("count.n", "w");
    outs = fopen("count.s", "w");
    getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], COUNT);
    getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], COUNT);
    fwrite(fld_n, sizeof(unsigned char), north_pts, outn);
    fwrite(fld_s, sizeof(unsigned char), south_pts, outs);
    fclose(outn);
    fclose(outs);
  #endif

  return 0;

}

#include "amsr_hires.C"
#include "amsr_getfld.C"
#include "amsr_icetools.C"
#include "amsr_process_bufr.C"
#include "pole_fill.C"
