#include <stdio.h>
#include "amsr2.h"

int main(int argc, char *argv[]) {
  amsr2head head;
  amsr2_spot s[12];
  amsr2_hrpt hr;
  amsr2_lrpt lr;
  
  int i, nread = 0, nobs, outfreq = 100000, nhigh = 0, nlow = 0;
  FILE *fin;
  fin = fopen("sat", "r");

  rewind(fin);
  while (!feof(fin) ) {
    fread(&head, sizeof(head), 1, fin);
    nobs = head.nspots;
    //if (nread % outfreq == 0) printf("nspots = %d\n",nobs);

    fread(&s[0], sizeof(amsr2_spot), nobs, fin);
    if (nobs == 12) { // low resolution spots
      nlow += 1;
      lr.head = head;
      for (i = 0; i < nobs; i++) { lr.obs[i] = s[i]; }
      for (i = 0; i < nobs; i += 2) {
      //printf("%5.1f %5.2f %3.1f %f %6.2f , %5.1f %5.2f %3.1f %f %6.2f\n",
      printf("%5.1f %5.2f %3.1f %6.2f , %5.1f %5.2f %3.1f %6.2f\n",
                                lr.obs[i].sccf/10.,
                                lr.obs[i].alfr,
                                lr.obs[i].anpo,
                                //lr.obs[i].viirsq,
                                lr.obs[i].tmbr,
                                lr.obs[i+1].sccf/10.,
                                lr.obs[i+1].alfr,
                                lr.obs[i+1].anpo,
                                //lr.obs[i+1].viirsq,
                                lr.obs[i+1].tmbr    );
      }
    }
    if (nobs == 2) {
      hr.head = head;
      nhigh += 1;
      for (i = 0; i < nobs; i++) { hr.obs[i] = s[i]; }
      printf("h %5.1f %5.2f %3.1f %6.2f  ",
                                hr.obs[0].sccf/10.,
                                hr.obs[0].alfr,
                                hr.obs[0].anpo,
                                //hr.obs[0].viirsq,
                                hr.obs[0].tmbr    );
      printf("h %5.1f %5.2f %3.1f %6.2f\n",
                                hr.obs[1].sccf/10.,
                                hr.obs[1].alfr,
                                hr.obs[1].anpo,
                                //hr.obs[1].viirsq,
                                hr.obs[1].tmbr    );
    }
    //else {
    //  printf("ERROR: unknown obs/spot type, nobs = %d\n",nobs);
    //}
                              
    nread += 1;
  }
  printf("nr = %d nread\n",nread);

  return 0;
}
