#include <stdio.h>
#include "sigridaa.h"

/* Program to read in SIGRID information */
/* Based loosely on icepick.f */
/* This version is for reading Antarctic sigrid files */

/* Function Prototypes */
int read_k(FILE *test, FILE *outs, FILE *lists, int *k_ndatarecs, \
           int *k_mesh_ratio, int *k_latoffset, int *k_lonoffset, \
           int *k_ngpts, int *exp_ptcnt, int *krec_count, int *rec_count, \
           int a);

void read_data(int i, char *rline, int *rec_count, int *drec_count, FILE *test,
  int a);

void process(char *rec1, FILE *lists, int a, int b);

void trimnull(char *rec1, char *rec2);

int strpos(char *a, char b, int i);

void dg(int start, int end, int *dgrecs, char *rec1, FILE *lists);

void writerec(int count, char *value, int precs, FILE *lists);

/* Global variables */
char inf_par[10][2];
int code_count[10], inf_parn;
int inf_lat, inf_long, mesh_spac, k_latoffset, k_mesh_ratio;
FILE *outs;

void main(argc, argv)
int argc; 
char *argv[];
{
/* Declarations */
FILE *fopen(), *test, *lists ;
char debug_name[14], output_name[12];
char line[MAXLINE], rline[MAXBLK][MAXLINE];
int rec_count, krec_count, drec_count, exp_ptcnt, ptcnt;
int i, j, k;
int inf_type, inf_mglm, inf_mgpp, inf_mesh;
char str2[10][2], str20[20], str11[11], date[7];
int k_ndatarecs, k_lonoffset, k_ngpts;
int result;

/* Set up file information */
test = fopen(*(argv+1),"r");

/* Set up various constants */
rec_count = 0;
krec_count = 0;
drec_count = 0;
exp_ptcnt = 0;
ptcnt = 0;

for (i = 0 ; i < 10 ; i++) {
    code_count[i] = 0; }

/* Get the SIGRIDINF header line ; subroutine read_inf in icepick*/
  fgets(line, MAXLINE, test);
  fputs(line, outs);
  sscanf(line, "%20s%1d%4d%5d%2s%3d%4d%2s%4d%2s%2d",str20, &inf_type, \
               &inf_lat, &inf_long, str2[0][0], \
               &inf_mglm, &inf_mgpp, str2[1][0], &inf_mesh, str2[2][0], 
               &inf_parn);
/* Derive the mesh spacing in hundredths of a degree*/
  mesh_spac = (inf_mesh*100)/60;
/* Pull out the codes */
  for (i=0 ; i < 10 ; i++)
    { j = 50 + 2*(i-1) - 1;
      inf_par[i][0] = line[j];
      inf_par[i][1] = line[j+1];
    }


/* Read in the SIGRIDNN header record, read_nn in icepick 
   Also open up the output files using the date from sigridnn */

  fgets(line, MAXLINE, test);
  for (i = 11; i <= 16; i++)
  { date[i-11] = line[i];
  }
  date[6] = '\0';
  sprintf(debug_name,"adebug.%s",date);
  sprintf(output_name,"aice.%s",date);
  debug_name[13] = '\0';
  output_name[11] = '\0';
  outs = fopen(debug_name,"w");
  lists = fopen(output_name,"w");
  printf("%s\n",date);
  fprintf(outs,"Type: %d \n", inf_type);
  fprintf(outs,"Origo Lat: %d \n", inf_lat);
  fprintf(outs,"Origo Lon: %d \n", inf_long);
  fprintf(outs,"Max. Merid. Grid lines: %d \n", inf_mglm);
  fprintf(outs,"Max Parallel Grid Points: %d \n", inf_mgpp);
  fprintf(outs,"Mesh, deg, min %d \n", inf_mesh);
/* Print out the codes */
  for (i=0 ; i < 10 ; i++)
    { j = 50 + 2*(i-1) - 1;
      fprintf(outs,"code %d  %2s \n", i, inf_par[i]); 
    }
/* Print the sigridnn record */
  fputs(line, outs);
  rec_count = rec_count+1;

/* Now execute main loop reading in the data */
  result = 0;
  while (!feof(test) && result == 0)
  {
    result = read_k(test, outs, lists, &k_ndatarecs, &k_mesh_ratio, \
      &k_latoffset, &k_lonoffset, &k_ngpts, &exp_ptcnt, &krec_count, \
      &rec_count, MAXLINE);
    if (result != 0) {fprintf(outs,"failure in read_k \n"); }
     else
     {
      for (i = 0; i < k_ndatarecs; i++)
      {
        read_data(i, &rline[i][0], &rec_count, &drec_count, test, MAXLINE);
      }
      process(&rline[0][0], lists, MAXBLK, MAXLINE);
     } 
  }

  fclose(lists);
  fclose(outs);
  fclose(test);

}
