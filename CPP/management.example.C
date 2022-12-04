#include <unistd.h>
#include "grid_math.h"

template <class T>
class a : public grid2<T> {
  protected:
    static int acount;
    static grid2<float> temp;
    int xsize(grid2<float> &x);
  public:
    a(void);
};
template <>
grid2<float> a<float>::temp ;
template <>
int a<float>::acount = 0;

template <class T>
a<T>::a(void) {
}
template <class T>
int a<T>::xsize(grid2<float> &x) {
  printf("x size %d\n",x.xpoints() );
  return x.xpoints();
}

////////////////////////////////////////////////////////
template <class T>
class b: public a<T> {
  protected:
    static int count;
    static grid2<float> x;
  public:
    b(void);
    int xsize(void) { a<T>::xsize(x); return 0;};
};
template<>
int b<float>::count = 0;
template<>
grid2<float> b<float>::x;
template <class T>
b<T>::b(void) {
  ijpt loc;
  x.resize(256,256);
  x.set((float) 0.0);
  if (this->acount == 0) { 
    this->temp.resize(10240/2,10240/2);
    printf("Resized temp, nx = %d\n",this->temp.xpoints() );
    for (loc.j = 0; loc.j < this->temp.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < this->temp.xpoints(); loc.i++) {
       this->temp[loc] = loc.i+loc.j;
    }
    }
  }
  this->acount += 1;
  count += 1;
  //printf("count = %d\n",count);
  fflush(stdout);
}

////////////////////////////////////////////////////////
template <class T>
class d: public a<T> {
  protected:
      static int count;
    public:
      d(void);
};
template <class T>
d<T>::d(void) {
}
////////////////////////////////////////////////////////
template <class T>
class c: public a<T> {
  protected:
    static grid2<float> x;
    static int count;
  public:
    c(void);
    int xsize(void) { a<T>::xsize(x); return 0;};
};
template<>
int c<float>::count = 0;
template<>
grid2<float> c<float>::x;
template <class T>
c<T>::c(void) {
  ijpt loc;
  x.resize(2560,2560);
  x.set((float) 0.0);
  if (this->acount == 0) { 
    this->temp.resize(10240/2,10240/2);
    printf("Resized temp, nx = %d\n",this->temp.xpoints() );
    for (loc.j = 0; loc.j < this->temp.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < this->temp.xpoints(); loc.i++) {
       this->temp[loc] = loc.i+loc.j;
    }
    }
  }
  this->acount += 1;
  count += 1;
  //printf("count = %d\n",count);
  fflush(stdout);
}

#define NGRIDS 1000
int main(void) {
  b<float> xx[NGRIDS];
  c<float> ac;
  d<float> ad;
  //int i;

  //for (i = 0; i < NGRIDS; i++) {
  //  printf("i, n %d %d\n",i, xx[i].temp.xpoints() );
  //  fflush(stdout);
  //}
  xx[0].xsize();
  ac.xsize();

  //sleep(15);
  return 0;
}
