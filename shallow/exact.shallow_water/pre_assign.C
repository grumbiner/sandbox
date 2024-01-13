
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

  tmpeta = eta[np];         // operator=

// Centered in time 
  eta[np] = eta[nm];        // operator=
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

  eta[nm] = eta[n];     // operator=
  eta[n] = tmpeta;      // operator=

  #ifdef DIAGNOSTIC
      i = nbase;
      DTYPE urms = u.rms(), vrms = v.rms(), etarms = eta[i].rms();
      printf("%d gridmax %f %f %f  rms %e %e %e energy %e u-v %e\n", i, 
          u.gridmax(), v.gridmax(), eta[i].gridmax(), urms, vrms, etarms, 
          gee*etarms*etarms + urms*urms + vrms*vrms, urms - vrms);
      //printf("%d rms %e %e %e energy %e u-v %e\n", i, urms, vrms, etarms, 
      //    gee*etarms*etarms + urms*urms + vrms*vrms, urms - vrms);
  #endif

  return;
}
