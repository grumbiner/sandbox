// Former element of the metricgrid --> mercator class, retired 2002/06/18
   void nicread(FILE *);
template <class T>
void mercator<T>::nicread(FILE *fin ) {
// routine to decode the nic files to ice concentrations
  int maxlen=1050*2*5;
  int nichead=6; //size of the nic header
  char *strtmp, *toktmp, *null;
  char delim;
  char **endptr;
  int i, j;
  float tmpx;

  delim = ' ';
  null = NULL;
  printf("maxlen = %d\n",maxlen);
  strtmp = new char[maxlen];
  toktmp = new char[maxlen];
  if (strtmp == NULL || toktmp == NULL) {
    cout << "failed to new temporary strings\n"; fflush(stdout);
  }
//part 1: decode in to sigrid numbers
  cout << "NIC headers\n";
  for (i = 0; i < nichead; i++) {
    fgets(strtmp, maxlen, fin);
    printf("%s",strtmp);
  }

  for (j = 0; j < ny; j++) {
    fgets(strtmp, maxlen, fin);
    toktmp = strtok(strtmp, &delim);
    //tmpx = strtod(toktmp, endptr); 
    tmpx = atof(toktmp);
    grid[0 + j*nx] = tmpx;
    for (i = 1; i < nx; i++) {
      toktmp = strtok(null, &delim);
      //tmpx = strtod(toktmp, endptr); 
      tmpx = atof(toktmp);
      grid[ i + j*nx] = tmpx;
    }
  }

//part 2: change sigrids into concentrations

}

