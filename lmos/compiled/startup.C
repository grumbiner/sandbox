
  long int seed = 0;
  srand48(seed);

// get the data:
  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  for (j = 0; j < 10; j++) {
    matchups[j].resize(NSTEP);
  }

  for (i = 0; i < NSTEP && !feof(fin); i++) {
    fscanf(fin, "%f %f %f %f %f %f %f %f %f %f\n", &tmp[0], &tmp[1],
        &tmp[2], &tmp[3], &tmp[4], &tmp[5], &tmp[6], &tmp[7], &tmp[8], &tmp[9]);
    for (j = 0; j < 10; j++) {
      matchups[j][i] = tmp[j];
    }
    if (feof(fin)) break;
  }
  int nobs = i;
  printf("nobs = %d\n",nobs); fflush(stdout);

  // info about initial columns -- mean, rms, variance
  for (j = 1; j < 10; j++) {
    printf("%2d %6.2f %6.2f %5.2f\n",j, matchups[j].average(),
            matchups[j].rms(),
            sqrt(-matchups[j].average()*matchups[j].average() +
                  matchups[j].rms()*matchups[j].rms())          );
    fflush(stdout);
  }
