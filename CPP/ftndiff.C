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
    norths[i-1].ftnin(fin);
    fclose(fin);
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
    printf("grid %d named %s\n",i, argv[i+1]);
    histo(norths[i], argv[i+1]);
  }
  printf("grid %d named %s\n",argc-2, argv[argc-1]);
  histo(norths[argc-2], argv[argc-1]);

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
    if (norths[i][loc] != norths[j][loc]) count += 1;
  }
  }
  printf("%d and %d differ %d times\n",i,j,count); fflush(stdout);
  return;
}

// Variant to ensure bathymetry doesn't choke the program:
void histo(GTYPE<DTYPE> &norths, char *fname) {
  mvector<int> counts(norths.gridmax() - norths.gridmin() + 1);
  ijpt loc;
  int tmin, tmax;

  for (int j = 0 ;j < counts.xpoints(); j++) {
    counts[j] = 0;
  }
  //counts = 0;
  tmin = norths.gridmin();

  printf("grid max, min %f %f counts.xpoints %d\n",norths.gridmax() , norths.gridmin(), counts.xpoints() );

  for (loc.j = 0; loc.j < norths.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < norths.xpoints(); loc.i++) {
     counts[ (int) norths[loc] - tmin ] += 1;
  }
  }

  for (int i = 0; i < counts.xpoints() ; i++) {
    if (counts[i] != 0) {
      printf("%f %d\n",(float) i-(float) tmin, counts[i] );
    }
  }

  return;
}
