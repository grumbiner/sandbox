#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

class grid {
public:
  int nx, ny;
  float *dat;
  grid();
  grid operator+(grid );
  void operator+=(grid );
  void operator=(grid );
};
grid::grid() {
  nx = 100;
  ny = 100;
  dat = (float *) malloc(nx*ny*sizeof(float) ) ;
}
void grid::operator=(grid y) {
  memcpy(dat, y.dat, nx*ny*sizeof(float) );
  return;
}

void grid::operator+=(grid y) {
  int i, j;
  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    dat[i+j*nx] += y.dat[i+j*nx];
  } 
  } 
  return;
}
  

grid grid::operator+(grid y) {
  grid scratch;
  int i, j;
  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    scratch.dat[i+j*nx] = dat[i+j*nx] + y.dat[i+j*nx];
  } 
  } 
  return scratch;
}

int main(void) {
  grid x, y;
  grid z;
  int i;

  for (i = 0; i < 1000; i++) {
   z = x;
   z += y;
  }
  printf("Going to sleep for a minute so that you can see if memory was released\n"); fflush(stdout);
  sleep (60);

  return 0;
}
