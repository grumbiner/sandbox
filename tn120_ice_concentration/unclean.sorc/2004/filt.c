/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */

float gr37(const ssmi *map, const int i, const int j,
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j,
           const int nx, const int ny, const int range);

int newfilt(ssmi *nmap, ssmi *smap) {
  int i, j;
  float *g37;
  int debug;
  unsigned char *nconc, *sconc;
  int index, indexip1, indexim1, indexjp1, indexjm1;

  debug = (0 == 1);
  #ifdef VERBOSE
  printf("entered newfilt\n"); fflush(stdout);
  #endif

  nconc = malloc(sizeof(unsigned char)*NX_NORTH*NY_NORTH);
  sconc = malloc(sizeof(unsigned char)*NX_SOUTH*NY_SOUTH);
  g37 = malloc(sizeof(float)*NX_NORTH*NY_NORTH);

  getfld(nmap, NX_NORTH*NY_NORTH, nconc, g37, BAR_CONC);
  getfld(smap, NX_SOUTH*NY_SOUTH, sconc, g37, BAR_CONC);
  #ifdef VERBOSE
    printf("in newfilt returned from getfld\n"); fflush(stdout);
  #endif


/* Find the northern hemisphere gradient ratio */
  for (j = 0; j < NY_NORTH  ; j++) {
    for (i = 0; i < NX_NORTH  ; i++) {
      index = i+j*NX_NORTH;
      #ifdef VERBOSE2
      printf("index = %d\n", index); fflush(stdout);
      #endif
      g37[index] = gr37(nmap, i, j, NX_NORTH, NY_NORTH, 0);
    }
  }
/* Loop over all points.  If, in any case, the 2 pt averaged gradient ratio
     for 37-19 v is greater than the cut off, then filter out the ice
     concentration */

  for (j = 1; j < NY_NORTH - 1 ; j++) {
    for (i = 1; i < NX_NORTH - 1 ; i++) {
      index = i+j*NX_NORTH;
      indexip1 = index+1;
      indexim1 = index-1;
      indexjp1 = index+NX_NORTH;
      indexjm1 = index-NX_NORTH;

      if (nconc[index] != 0 && nconc[index] != BAD_DATA) {

        if (nconc[indexjp1] != BAD_DATA) {
          if (g37[indexjp1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexjm1] != BAD_DATA) {
          if (g37[indexjm1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexim1] != BAD_DATA) {
          if (g37[indexim1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexip1] != BAD_DATA) {
          if (g37[indexip1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }

      } /* End of filtration testing */

    }
  }

  for (index = 0; index < NY_NORTH*NX_NORTH; index++) {
     nmap[index].bar_conc = nconc[index];
  }

/* Need to put southern filtering in here */
  for (j = 0; j < NY_SOUTH  ; j++) {
    for (i = 0; i < NX_SOUTH  ; i++) {
      index = i+j*NX_SOUTH;
      g37[index] = gr37(smap, i, j, NX_SOUTH, NY_SOUTH, 0);
    }
  }
/* Note above that we've used the same array for both north and south
   gradients */
  for (j = 1; j < NY_SOUTH - 1; j++) {
    for (i = 1; i < NX_SOUTH - 1; i++) {
      index = i+j*NX_SOUTH;
      indexip1 = index+1;
      indexim1 = index-1;
      indexjp1 = index+NX_SOUTH;
      indexjm1 = index-NX_SOUTH;
      #ifdef VERBOSE2
        printf("south, i, j, index = %d %d %d\n",i,j,index); fflush(stdout);
      #endif

      if (sconc[index] != 0 && sconc[index] != BAD_DATA) {

        if (sconc[indexjp1] != BAD_DATA) {
          if (g37[indexjp1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexjm1] != BAD_DATA) {
          if (g37[indexjm1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexim1] != BAD_DATA) {
          if (g37[indexim1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexip1] != BAD_DATA) {
          if (g37[indexip1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }

      } /* End of filtration testing */

    }
  }
  #ifdef VERBOSE
  printf("finished filtration loop\n"); fflush(stdout);
  #endif

  for (j = 0; j < NY_SOUTH*NX_SOUTH; j++) {
     smap[j].bar_conc = sconc[j];
  }
  #ifdef VERBOSE
  printf("newfilt finished smap loop\n"); fflush(stdout);
  #endif

  free(sconc);
  free(nconc);
  free(g37);
  #ifdef VERBOSE
  printf("about to return from newfilt\n"); fflush(stdout);
  #endif
  return 0;
}

float gr37(const ssmi *map, const int i, const int j,
                            const int nx, const int ny, const int range)
{
   int index, ti, tj, count;
   float t19v, t37v, tempor;

   #ifdef VERBOSE2
   printf("in gr37, i, j, nx, ny, range = %d %d %d %d  %d\n",i,j,nx,ny,range);
   fflush(stdout);
   #endif

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


float gr22(const ssmi *map, const int i, const int j,
                            const int nx, const int ny, const int range)
{
   float t19v, t22v, tempor;
   int index, ti, tj, count;

   t19v = 0.0;
   t22v = 0.0;
   count = 0;

   for (tj = j-range ; tj < j+range ; tj++) {
     for (ti = i - range ; ti < i+range; ti++) {
        count += 1;
        index = i + j*nx;
        if (index < 0 || index >= nx*ny) continue;
        t19v += map[index].t19v;
        t22v += map[index].t22v;
     }
   }
   t19v = t19v / count;
   t22v = t22v / count;

   tempor = (t22v - t19v) / (t22v + t19v);

   return tempor;
}

