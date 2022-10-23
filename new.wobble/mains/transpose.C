#include "grid_math.h"

#define NPER 12

template<class T>
class t382 : public grid2<T> {
  public:
    t382();
};
template<class T>
t382<T>::t382() {
  this->nx = 1152;
  this->ny =  576;
  this->grid = new T[this->nx*this->ny];
} 

int main(int argc, char *argv[]) {
  t382<float> tmp;
  t382<double> tmp2;
  ijpt loc, tloc;
  FILE *fin, *fout;
  int i;
  
  fin = fopen(argv[1],"r");
  fout = fopen(argv[2], "w");
  tmp.binin(fin);
  tmp.binout(fout);

  for (i = 0; i < NPER; i++) {
    tmp2.binin(fin);
    conv(tmp2, tmp);
    tmp.binout(fout);
  }
  for (i = 0; i < NPER; i++) {
    tmp.binin(fin);
    tmp.binout(fout);
  }

  fclose(fout);
  fclose(fin);

  return 0;

}
