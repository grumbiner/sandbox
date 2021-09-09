#include <ncepgrids.h>

void compare(int argc, char *argv[]) ;
bool match(GTYPE<DTYPE> *norths, int i, int j) ;
void differ(GTYPE<DTYPE> *norths, int i, int j) ;
void histo(GTYPE<DTYPE> &norths, char *fname) ;

int main(int argc, char *argv[]) {
  
  compare(argc, argv);

  return 0;
}
void compare(int argc, char *argv[]) {
  GTYPE<DTYPE> norths[argc-1];
  FILE *fin;
  int i, j;

  for (i = 1; i < argc; i++) {
    printf("%s\n",argv[i]);
    fin = fopen(argv[i], "r");
    norths[i-1].binin(fin);
    fclose(fin);
  }

  for (i = 0; i < argc-1; i++) {
    printf("grid %d named %s\n",i, argv[i+1]);
    histo(norths[i], argv[i+1]);
  }

  for (i = 0; i < argc-2; i++) {
    for (j = i+1; j < argc-1; j++) {
      if (match(&norths[0], i, j)) {
        printf("grids %d and %d match, names %s and %s\n",
           i,j,argv[i+1], argv[j+1]);
      }
      else {
        differ(&norths[0], i, j);
      }
    }
  }

  return;
} 
bool match(GTYPE<DTYPE> *norths, int i, int j) {
  ijpt loc;

  for (loc.j = 0; loc.j < norths[i].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < norths[i].xpoints(); loc.i++) {
    if (norths[i][loc] != norths[j][loc]) return false;
  }
  }
  return true;
}
void differ(GTYPE<DTYPE> *norths, int i, int j) {
  int count = 0;
  ijpt loc;
  for (loc.j = 0; loc.j < norths[i].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < norths[i].xpoints(); loc.i++) {
    if (norths[i][loc] != norths[j][loc]) {
       count += 1;
      //printf("%d %d  %d %d\n",loc.i, loc.j, (int)norths[i][loc], (int)norths[j][loc]);
    }
  }
  }
  printf("%d and %d differ %d times\n",i,j,count); fflush(stdout);
  return;
}
void histo(GTYPE<DTYPE> &norths, char *fname) {
  mvector<int> counts(256);
  ijpt loc;
  counts = 0;
  
  if (norths.gridmax() > 256 || norths.gridmin() < 0) {
    printf("%s doesn't look like a landmask, max is %f min is %f\n",fname, (float) norths.gridmax(), norths.gridmin() );
  }
  else {
    if (norths.gridmax() < 2.56) {
      norths *= 100.;
    }
  } 


  for (loc.j = 0; loc.j < norths.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < norths.xpoints(); loc.i++) {
     counts[ (int) norths[loc] ] += 1;
  }
  }

  for (int i = 0; i < 256; i++) {
    if (counts[i] != 0) {
      printf("%3d %d\n",i, counts[i]);
    }
  }

  return;
}
