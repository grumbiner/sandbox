// Robert Grumbine 10 December 2004
//
#ifndef GRIDH
  #include "grid_math.h"
#endif


template<class T>
class skillfield : public grid2<T> {
  public:
    skillfield();
    skillfield(int, int);
    float pod(skillfield &);
    float far(skillfield &);
    float pid(skillfield &, grid2<T> &);
    float fir(skillfield &, grid2<T> &);
    float correct_info(skillfield &, grid2<T> &);
};
template<class T>
skillfield<T>::skillfield(void) {
  grid = (T *) NULL;
  nx = 0;
  ny = 0;
  #ifdef VERBOSE
    cout << "Leaving skillfield::skillfield(void)\n";
  #endif
}
template<class T>
skillfield<T>::skillfield(int n1, int n2) {
  if (nx == 0 && ny == 0) {
  #ifdef VERBOSE
    cout <<"Calling grid2_base resizer from skillfield\n"; fflush(stdout);
  #endif
    skillfield<T>::resize(n1, n2); //Note that this is actually invoking 
                                   //the grid2_base constructor, which is 
                                   //what we want, but may look a little odd
  }
  #ifdef VERBOSE
    printf("In skillfield, nx ny = %d %d\n",nx, ny);
  #endif
}
// Probability that invoker has a nonzero value where y does (predict nonzero in y)
template<class T>
float skillfield<T>::pod(skillfield<T> &y) {
  int nx = y.xpoints(), ny = y.ypoints();
  int predict = 0, present = 0;
  int i;
  for (i = 0; i < nx*ny; i++) {
    if (y[i] != 0) {
      present += 1;
      if (this->operator[](i) != 0) {
        predict += 1;
      }
    }
  }
  return (float) predict / (float) present;
}
// Probability that invoker has a nonzero value where y does (predict nonzero in y)
template<class T>
float skillfield<T>::far(skillfield<T> &y) {
  int nx = y.xpoints(), ny = y.ypoints();
  int predict = 0, not_present = 0;
  int i;
  for (i = 0; i < nx*ny; i++) {
    if (this->operator[](i) != 0) {
      predict += 1;
      if (y[i] == 0) {
        not_present += 1;
      }
    }
  }
  return (float) not_present/ (float) predict;
}
//Fraction of the bits of information in the y grid (given probability 
//grid in argument) that the this grid explains -- at points that do have
//ice
template<class T>
float skillfield<T>::pid(skillfield<T> &y, grid2<T> &prob) {
  int nx = y.xpoints(), ny = y.ypoints();
  float predict = 0, present = 0;
  int i;
  for (i = 0; i < nx*ny; i++) {
    if (y[i] != 0) {
      present += 1 * (-1.)*prob[i]*log(prob[i])/log(2.);
      if (this->operator[](i) != 0) {
        predict += 1  * (-1.)*prob[i]*log(prob[i])/log(2.);
      }
    }
  }
  return predict / present;
}
// Probability that invoker has a nonzero value where y does (predict nonzero in y)
template<class T>
float skillfield<T>::fir(skillfield<T> &y, grid2<T> &prob) {
  int nx = y.xpoints(), ny = y.ypoints();
  float predict = 0, not_present = 0;
  int i;
  for (i = 0; i < nx*ny; i++) {
    if (this->operator[](i) != 0) {
      predict += 1;
      if (y[i] == 0) {
        if (prob[i] != 0) {
          not_present += 1 / (0.5 - prob[i]*log(prob[i])/log(2.)) ;
        }
        else {
          not_present += 1. / (0.5 - 0.);
        }
      }
    }
  }
  if (predict == 0) {
    printf("predict in fir = 0\n");
    return 0.0;
  }
  else {
    return not_present/ predict;
  }
}
template<class T>
float skillfield<T>::correct_info(skillfield<T> &y, grid2<T> &prob) {
  float totinfo = 0, correct = 0.;
  int i;
  for (i = 0; i < prob.xpoints()*prob.ypoints(); i++) {
    if (prob[i] != 0) {
      totinfo += - prob[i]*log(prob[i])/log(2.);
      if ( (this->operator[](i) > 0. && y[i] > 0.) ||
           (this->operator[](i) == 0. && y[i] == 0.) ) {
        correct +=  - prob[i]*log(prob[i])/log(2.);
      }
    }
  }

  //printf("correct %f total %f\n",correct, totinfo);
  return correct / totinfo;
}
