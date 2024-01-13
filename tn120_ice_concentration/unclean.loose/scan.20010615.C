#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"

#include "date.h"

int tailclean(int j, Date &current, char *base, southgrid<unsigned char> *ice) ;

int main(int argc, char *argv[]) {
  char *base, *tag, *fname;
  southgrid<unsigned char> ice[31];
  int yy, mm, dd;
  Date *current;
  int i, j = 0;
  FILE *fin;

// Deal with arguments and initialize date:
  if (argc < 5) {
    printf("need base, yy, mm, dd\n");
    return 1;
  }
  base = argv[1];
  yy = atoi(argv[2]);
  mm = atoi(argv[3]);
  dd = atoi(argv[4]);
  current = new Date (yy,mm,dd);
  fname = new char[800];

// Loop through and demonstrate reading the data files:
  tag = current->show();
  printf("%s %d %d %d %s\n",base, yy, mm, dd, tag);
  fflush(stdout);
  for (i = 0; i < 778; i++) {
    sprintf(fname,"%s%s", base, &tag[2]);
    printf("%d %s\n",i , fname); fflush(stdout);
    fin = fopen(fname, "r");
    if (fin != (FILE *) NULL) { 
      ice[ j%31 ].binin(fin);
      fclose(fin);
      j+= 1;
    }
    else {
      printf("Missed %s \n",fname); fflush(stdout);
    }

    if (j >= 31) {
      printf("calling tailclean, j = %d\n",j); fflush(stdout);
      tailclean(j, *current, base, ice); // routine to clean up the tailing file
      printf("returned from tailclean %d\n",j); fflush(stdout);
    }

    // advance the date.  Nicer would be to pass in date to end at.
    current->nextday();
    tag = current->show();
  } 

  printf("j at end %d\n",j);

  return 0;
}

// routine to clean up the tailing file
// given data sources, bad == no_data == weather
// examine the most recent file (indexed by j % 31) and
//   *) Leave as ice concentration if currently an ice concentration 
//        and the values in the last 31 looks aren't overwhelmingly 'bad'
//        and the values don't flop back and forth between bad/0 ice and ice
//            too much
//   *) If currently 'bad' fill in with last ice concentration if
//        values don't flop too much
// Note: Do not change the full file, just the output variant.
#define MAX_ICE 128
#define BAD_MAX 28
int tailclean(int j, Date &current, char *base, southgrid<unsigned char> *ice) {
   southgrid<unsigned char> clean;
   //vector<unsigned char> tmp(31);
   ijpt loc;
   int i, bad, change = 0, checking = j % 31;
   char *tag, *fname;
   FILE *fout;

   fname = new char[800];
   tag = current.show();
   clean = ice[checking];
   for (loc.j = 0; loc.j < ice[0].ypoints(); loc.j++) {
   for (loc.i = 0; loc.i < ice[0].xpoints(); loc.i++) {
      //for (i = 0; i < 31; i++) {
      //   tmp[i] = ice[i][loc];   
      //}
      bad = 0;
      for (i = 0; i < 31; i++) {
        if (ice[i][loc] > MAX_ICE) bad += 1;
      }
      if (bad > BAD_MAX && clean[loc] > 0 && clean[loc] < MAX_ICE) {
        clean[loc] = 0;
        change += 1;
        printf("revised %d %d\n",loc.i, loc.j); 
      } 
   }
   } 

   printf("%s%s.new",base,&tag[2]); fflush(stdout);
   sprintf(fname, "%s%s.new",base,&tag[2]);
   printf("trying to open %s\n",fname);
   fout = fopen(fname, "w+");
   if (fout == (FILE *) NULL) {
     printf("failed to open output file %s\n",fname);
     return -1;
   }
   clean.binout(fout);
   fclose(fout);

   return change;
}
