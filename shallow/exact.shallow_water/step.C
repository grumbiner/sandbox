
void step(int nbase, grid2<DTYPE> &u, grid2<DTYPE> &v, grid2<DTYPE> *eta, grid2<DTYPE> &tmpeta, grid2<DTYPE> &h) {
  int i, n, np, nm;
  ijpt loc, locip, locjp, locim, locjm;
  int nx = h.xpoints(), ny = h.ypoints() ;

  if (nbase == 2) {
    n = 2;
    np = 0;
    nm = 1;
  }
  else if (nbase == 1) {
    n = 1;
    np = 2;
    nm = 0;
  }
  else if (nbase == 0) {
    n = 0;
    np = 1;
    nm = 2;
  }
  else {
    printf("nbase is out of range, = %d\n",nbase);
    return;
  }

  //for (i = 0; i < nx*ny; i++) {
  //  tmpeta[i] = eta[np][i];         // operator=
  //}
  tmpeta = eta[np];

// Centered in time 
  //for (i = 0; i < nx*ny; i++) {
  //  eta[np][i] = eta[nm][i];        // operator=
  //}
  eta[np] = eta[nm];


// Forward in time on u,v

  DTYPE c0 = 2.*dt/2./dx;
  DTYPE c1 = gee * dt / 2. / dx;

  for (loc.j = 1; loc.j < ny - 1; loc.j++) {
    locim.j = loc.j;
    locip.j = loc.j;
    locjm.j = loc.j - 1;
    locjp.j = loc.j + 1;
    for (loc.i = 1; loc.i < nx - 1; loc.i++) {
      locim.i = loc.i - 1;
      locip.i = loc.i + 1;
      locjm.i = loc.i ;
      locjp.i = loc.i ;
      // centered, but ensure we advance eta before u,v as u,v are only
      //   keeping 1 time level, overwriting the previous:
      eta[np][loc] -= c0 * ( 
         (u[locip]*h[locip] - u[locim]*h[locim])  + 
         (v[locjp]*h[locjp] - v[locjm]*h[locjm])   ) ;
     }
  }

    // forward in u,v -- overwriting, so must keep in separate loop
  for (loc.j = 1; loc.j < ny - 1; loc.j++) {
    locim.j = loc.j;
    locip.j = loc.j;
    locjm.j = loc.j - 1;
    locjp.j = loc.j + 1;
    for (loc.i = 1; loc.i < nx - 1; loc.i++) {
      locim.i = loc.i - 1;
      locip.i = loc.i + 1;
      locjm.i = loc.i ;
      locjp.i = loc.i ;
      u[loc] -= c1 * (eta[n][locip] - eta[n][locim]);
      v[loc] -= c1 * (eta[n][locjp] - eta[n][locjm]);
    }
  }

  eta[nm] = eta[n];
  //for (i = 0; i < nx*ny; i++) {
  //  eta[nm][i] = eta[n][i];     // operator=
  //}
  
  eta[n]  = tmpeta;
  //for (i = 0; i < nx*ny; i++) {
  //  eta[n][i] = tmpeta[i];      // operator=
  //}

  return;
}
