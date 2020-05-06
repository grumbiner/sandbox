//Test notion of vectors as grid_math types, with ny = 1

#ifndef GRIDH
  #include "grid_math.h"
#endif

template <class T>
class v2 : public grid2<T> {
  public :
    T *vec;
    v2(void);
    ~v2();
    v2(int );
    v2(v2<T> &);
    
    v2<T> & operator=(v2<T> );
    v2<T> & operator=(grid2<T> );
};

template <class T>
v2<T>::~v2() {
  vec = (T *) NULL;  // Grid destruction will handle the rest
}
template <class T>
v2<T>::v2() {
  nx = 1;
  ny = 1;
  grid = (T *) malloc(nx*ny*sizeof(T) );
  vec  = grid;
}
template <class T>
v2<T>::v2(int n) {
  nx = n;
  ny = 1;
  grid = (T *) malloc(nx*ny*sizeof(T) );
  vec  = grid;
}
template <class T>
v2<T>::v2(v2<T> &x) {
  int i, j;
  nx = x.nx;
  ny = 1;    // Should test on x.ny
  grid = (T *) malloc(nx*ny*sizeof(T) );
  vec  = grid;
  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
     grid[i+j*nx] = x.grid[i+j*nx];
  }
  }
}

template <class T>
v2<T> & v2<T>::operator=(v2<T> x) {
  int i, j;
  nx = x.nx;
  ny = 1;    // Should test on x.ny
  grid = (T *) malloc(nx*ny*sizeof(T) );
  vec  = grid;
  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
     grid[i+j*nx] = x.grid[i+j*nx];
  }
  }
  return *this;
}
template <class T>
v2<T> & v2<T>::operator=(grid2<T> x) {
  int i, j;
  nx = x.xpoints();
  ny = 1;    // Should test on x.ny
  grid = (T *) malloc(nx*ny*sizeof(T) );
  vec  = grid;
  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
     grid[i+j*nx] = x.grid[i+j*nx];
  }
  }
  return *this;
}

