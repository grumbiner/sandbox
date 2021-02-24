#include "rgglob.h"

extern "C" float arcdis(float lon1, float lat1, float lon2, float lat2);

float distance(rgglob &u, ijpt &x, ijpt &y) ;
void blobber(rgglob *eta, latpt &ll, float blob_height, float sigma) ;

void shallow(rgglob &u, rgglob &v, rgglob *eta, rgglob &coriol, rgglob &dx, rgglob &dy, const float g, const float h, const float dt, int n1, int n2, int n3) ;

// cheating and perhaps very unadvisable using the 4th element of eta as a temporary array
//   to avoid creating unbounded local copies.  Proper would be to ensure that the 
//   deletion/destructor is correct.
int main(int argc, char *argv[]) {
  rgglob u, v, eta[4];
  rgglob coriol, dx, dy; // constant through run
  rgglob tmp;

  float g = 9.81, h = 40., omega = 7.292e-5, dt = 120.0;
  float radperdeg = M_PI / 180.0;
  float blob_height = 0.5;
  float hmax = 0.5, hmin = -0.15/16.;

  int ndays = 7, nbase = 1, np = 2, nm = 0, outfreq = 10;
  int i, k;

  latpt ll;
  ijpt loc;
  ijpt ip, jp;

  palette<unsigned char> gg(19, 65), hh(25,65);
  char fname[80];


// fields is pointers to the assorted metricgrids.  Better than trying to
//   remember which all grid names are involved.  Note, though, that the
//   locate invoked this way doesn't preserve/provide k.

  if (argc > 1) {
    dt = atof(argv[1]);
  }
  if (argc > 2) {
    ndays = atoi(argv[2]);
  }
  outfreq = max(1, (int) nearbyint((5.*86400.)/dt)) ;
  printf("time step = %f seconds %d day forecast output frequency = %d \n",dt, ndays, outfreq);
  for (i = 0; i < hh.ncol; i++) {
    hh.set_color(i,255-i*10, 255-i*10,255-i*10);
  }

// Set up coriolis term on grids, natively -- initialize all -------------------
  for (k = 0; k < NFIELDS; k++) {
    loc.k = k;
    u.fields[k]->set((float) 0.0);
    v.fields[k]->set((float) 0.0);
    for (i = 0; i < 4; i++) {
      eta[i].fields[k]->set((float) 0.0);
    }

    for (loc.j = 0; loc.j < u.fields[k]->ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < u.fields[k]->xpoints(); loc.i++) {
      ll = u.fields[k]->locate(loc);
      coriol[loc] =  2.*omega*sin(ll.lat*radperdeg);
    }
    }
    printf("%d: %e max %e min\n",k, coriol.fields[k]->gridmax(), coriol.fields[k]->gridmin() );
  }
  fflush(stdout);

// set up the dx, dy grids -------------------------------------------------------
  for (k = 0; k < NFIELDS; k++) {
    loc.k = k;
    ip.k  = k;
    jp.k  = k;
    for (loc.j = 0; loc.j < u.fields[k]->ypoints()-1; loc.j++) {
    ip.j = loc.j;
    jp.j = loc.j + 1;
    for (loc.i = 0; loc.i < u.fields[k]->xpoints()-1; loc.i++) {
      ip.i = loc.i + 1;
      jp.i = loc.i;
      dx[loc] = distance(u, loc, ip);
      dy[loc] = distance(u, loc, jp);
    }
    }
    printf("%d: dx %e max %e min %e average\n",k, dx.fields[k]->gridmax(), dx.fields[k]->gridmin(0.0),
          dx.fields[k]->average(0.0) );
    printf("%d: dy %e max %e min %e average\n",k, dy.fields[k]->gridmax(), dy.fields[k]->gridmin(0.0),
          dy.fields[k]->average(0.0) );
  }
  fflush(stdout);

  tmp = dx;
    //sprintf(fname,"dx.xpm");
    //tmp.fields[NH]->scale();
    //tmp.fields[NH]->xpm(fname,7,gg);
  tmp = dy;
    //sprintf(fname,"dy.xpm");
    //tmp.fields[NH]->scale();
    //tmp.fields[NH]->xpm(fname,7,gg);

// Now do some checking on the time step ------------------------------
  float tmin = 10*3600.0;
  tmp = dx;
  for (k = 0; k < NFIELDS; k++) {
    tmp.fields[k]->operator/= (sqrt(g*h)) ;
    printf("field %d dx time step %f %f %f\n",k, tmp.fields[k]->gridmax(0.), 
                    tmp.fields[k]->gridmin(0.), tmp.fields[k]->average(0.) );
    tmin = min(tmin, tmp.fields[k]->gridmin(0.) );
  }
  tmp = dy;
  for (k = 0; k < NFIELDS; k++) {
    tmp.fields[k]->operator/= (sqrt(g*h) );
    printf("field %d dy time step %f %f %f\n",k, tmp.fields[k]->gridmax(0.), tmp.fields[k]->gridmin(0.), tmp.fields[k]->average(0.) );
    tmin = min(tmin, tmp.fields[k]->gridmin(0.) );
  }

  printf("Minimum dt on the grids: %f\n",tmin);

  if (tmin < dt) {
    printf("Minimum dt is %f but you selected %f\n",tmin, dt);
    return 1;
  }

// ====================================================================
// ====================================================================
  
// Initialize a blob in the North atlantic (nps grid)
  ll.lat =  75.0;
  ll.lon = -80.0;
  blobber(eta, ll, blob_height, 333.0e3);
  printf("eta max, min %f %f\n",eta[0].gridmax(), eta[0].gridmin() ); fflush(stdout);
  printf("eta NH max, min %f %f\n",eta[0].fields[NH]->gridmax(), eta[0].fields[NH]->gridmin() ); fflush(stdout);
//  ll.lat = 0.0;
//  ll.lon = 0.0;
//  blobber(eta, ll, blob_height, 333.0e3);
//  ll.lat =  -70.0;
//  ll.lon =   0.0;
//  blobber(eta, ll, blob_height, 333.0e3);
//  printf("eta max, min %f %f\n",eta[0].gridmax(), eta[0].gridmin() ); fflush(stdout);
//  printf("eta SH max, min %f %f\n",eta[0].fields[SH]->gridmax(), eta[0].fields[SH]->gridmin() ); fflush(stdout);
//  printf("eta NH max, min %f %f\n",eta[0].fields[NH]->gridmax(), eta[0].fields[NH]->gridmin() ); fflush(stdout);
//  return 0;



// pointers for the time stepping -- eta is 3 levels, u,v are explicit
  for (i = 0; i < (int) (0.5+ ndays*(86400.)/dt)+1; i++) {
    nbase = (i+1)     % 3;
    np    = (nbase+1) % 3;
    nm    = (np + 1)  % 3;
    printf("%4d, time = %6.0f min ",i, (float)i*dt/60.); fflush(stdout);

    shallow(u, v, eta, coriol, dx, dy, g, h, dt, nm, nbase, np);

    k = np;
    printf("eta %6.4f %7.4f %e %e ",eta[k].fields[NH]->gridmax(), 
             eta[k].fields[NH]->gridmin(),eta[k].fields[NH]->average(), 
             eta[k].fields[NH]->rms() ); 
    fflush(stdout);
    printf(" u %6.4f %7.4f\n",u.fields[NH]->gridmax(), u.fields[NH]->gridmin() ); fflush(stdout);

    if (eta[np].fields[NH]->gridmax() > 5.0) break;
    if ( (i%outfreq) == 0) {
      sprintf(fname,"nh%04d.xpm",i/outfreq);
      tmp = eta[np];
      if (tmp.fields[NH]->gridmax() > hmax)      hmax *= 2.0;
      if (tmp.fields[NH]->gridmax() < hmax / 2.) hmax /= 2.0;
      if (tmp.fields[NH]->gridmin() < hmin)      hmin *= 2.0;
      if (tmp.fields[NH]->gridmin() > hmin / 2.) hmin /= 2.0;
      
      //tmp.fields[NH]->scale((float) -0.05, (float) 0.50); // early in the run
      tmp.fields[NH]->scale(hmin, hmax); // early in the run
      tmp.fields[NH]->xpm(fname,10,hh);
    }
  }

  FILE *fout;
  sprintf(fname,"%03d.out",(int)dt);
  fout = fopen(fname,"w");
  
  eta[np].fields[NH]->binout(fout);
  u.fields[NH]->binout(fout);
  v.fields[NH]->binout(fout);
  eta[np].fields[LL]->binout(fout);
  u.fields[LL]->binout(fout);
  v.fields[LL]->binout(fout);
  eta[np].fields[SH]->binout(fout);
  u.fields[SH]->binout(fout);
  v.fields[SH]->binout(fout);

  fclose(fout);

  return 0;
}
void shallow(rgglob &u, rgglob &v, rgglob *eta, rgglob &coriol, 
             rgglob &dx, rgglob &dy, 
             const float g, const float h, const float dt,
             int nm, int nbase, int np) {
  ijpt loc, ip, jp, im, jm;
  int k;

// eta[3] is a temporary array
  eta[3]  = eta[np];
  eta[np] = eta[nm]; // leapfrog in time for eta

// -- cyclicity checks
// -- land masking


// move from explicit mention (nh, sh, latlon) to field[k]-> references
  for (k = 0; k < NFIELDS; k++) {
  //k = 0;

  loc.k = k;
  ip.k  = k;
  jp.k  = k;
  im.k  = k;
  jm.k  = k;

  //tmp = eta - h*div(u,v)*dt;
  for (loc.j = 1; loc.j < u.fields[k]->ypoints()-1; loc.j++) {
    ip.j = loc.j;
    jp.j = loc.j + 1;
    im.j = loc.j;
    jm.j = loc.j - 1;
    for (loc.i = 1; loc.i < u.fields[k]->xpoints()-1; loc.i++) {
      ip.i = loc.i + 1;
      jp.i = loc.i;
      im.i = loc.i - 1;
      jm.i = loc.i;
      eta[np].fields[NH]->operator[](loc) -= dt*2.*h*( 
                       (u.fields[k]->operator[](ip) - u.fields[k]->operator[](im))/ (dx.fields[k]->operator[](loc)+dx.fields[k]->operator[](im) ) + 
                       (v.fields[k]->operator[](jp) - v.fields[k]->operator[](jm))/ (dy.fields[k]->operator[](loc)+dy.fields[k]->operator[](jm) )    );
    }
  }

  // put the u,v in a separate loop because we're overwriting them
  //u,v = u,v - g*grad(eta)*dt - fk x (uv);
  for (loc.j = 1; loc.j < u.fields[k]->ypoints()-1; loc.j++) {
    ip.j = loc.j;
    jp.j = loc.j + 1;
    im.j = loc.j;
    jm.j = loc.j - 1;
    for (loc.i = 1; loc.i < u.fields[k]->xpoints()-1; loc.i++) {
      ip.i = loc.i + 1;
      jp.i = loc.i;
      im.i = loc.i - 1;
      jm.i = loc.i;
      u.fields[k]->operator[](loc) += - g*dt*(eta[nbase].fields[k]->operator[](ip) - eta[nbase].fields[k]->operator[](im) )/(dx.fields[k]->operator[](loc)+dx.fields[k]->operator[](im)) 
                     + dt*coriol.fields[k]->operator[](loc)*v.fields[k]->operator[](loc);
      v.fields[k]->operator[](loc) += - g*dt*
              (eta[nbase].fields[k]->operator[](jp) - eta[nbase].fields[k]->operator[](jm) )/
              (dy.fields[k]->operator[](loc)+dy.fields[k]->operator[](jm))
          - dt*coriol.fields[k]->operator[](loc) * u.fields[k]->operator[](loc);
    }
  }

  } // k -- index of which field is being worked on

/////////////// Regardless of version:
  eta[nm]    = eta[nbase];
  eta[nbase] = eta[3];

  return;
}
float distance(rgglob &u, ijpt &x, ijpt &y) {
  latpt ll1, ll2;
  ll1 = u.fields[x.k]->locate(x);
  ll2 = u.fields[x.k]->locate(y);
  return 1000.*arcdis(ll1.lon, ll1.lat, ll2.lon, ll2.lat); 
}
//translate to geometric for sigma
void blobber(rgglob *eta, latpt &ll, float blob_height, float sigma) {
  ijpt loc, tloc;
  latpt ll2;
  float dist;
 // new, expect sigma to be meters  
  loc = eta[0].locate(ll);
  printf("k %d ll %f %f h %f sigma %f\n",loc.k, ll.lat, ll.lon, 
              blob_height, sigma);

  eta[0][loc] = blob_height;
  eta[1][loc] = blob_height;
  eta[2][loc] = blob_height;
  tloc.k = loc.k;

  for (float j = 0.0; j < eta[0].fields[loc.k]->ypoints() ; j += 1.0 ) {
  for (float i = 0.0; i < eta[0].fields[loc.k]->xpoints() ; i += 1.0 ) {
    tloc.j = (int) j;
    tloc.i = (int) i;

    ll2 = eta[0].locate(tloc);
    dist = 1000.*arcdis(ll.lon, ll.lat, ll2.lon, ll2.lat);
    
    eta[0][tloc] = blob_height*exp(-(dist*dist)/2./sigma/sigma);
    eta[1][tloc] = blob_height*exp(-(dist*dist)/2./sigma/sigma);
    eta[2][tloc] = blob_height*exp(-(dist*dist)/2./sigma/sigma);
  }
  }
  printf("location of blob: %d %d %d\n",loc.i, loc.j, loc.k);
  printf("eta max, min %f %f\n",eta[0].gridmax(), eta[0].gridmin() ); fflush(stdout);

  return;
}
