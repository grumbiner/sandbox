#ifndef PROGRAM_H

#define PROGRAM_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include "mvector.h"

// Maximum length of a program:
#define MAXLEN 1000

#define NMUTANTS 6
#define NDUP 4

#define INCREMENT 0
#define DECREMENT 1
#define MULTIPLY 2
#define DIVIDE 3
#define POINTSWAP 4
#define DUPLICATE 5

class program : public mvector<float> {
  private:
    int pgmlen;
  public:
    float value;
    program(int n=1) {pgmlen = n; setsize(MAXLEN); }
    void list(FILE *) ;
    int length()   {return pgmlen; }
    int resize(int );
    void set(program &);
    void mutate(program &x);
};
int program::resize(int n) {
  if (n < MAXLEN) {
    pgmlen = n;
    return (1==1);
  }
  else {
    printf("Failed to resize, greater than max %d\n",MAXLEN);
    return (0==1);
  }
}
void program::set(program &x) {
  pgmlen = x.pgmlen;
  value  = x.value;
  mvector<float>::operator=(x);
}
void program::list(FILE *fout) {
  int k;
  for (k = 0; k < pgmlen; k++) {
    fprintf(fout, "pc %d pgm %f\n", k, this->operator[](k) );
  }
  return;
}

//Routine to mutate the 'this' program and put the result in to x
void program::mutate(program &x) {
  float pmutate, prob, tfloat, nmut= (float) NMUTANTS;
  int i, j, k;
  int mutant_type, nmod;

  pmutate = max(1./pgmlen, 0.05);
  j = 0; nmod = 0;
  if (pgmlen != x.length() ) {
    x.resize(pgmlen);
  }

// Note that the evil comma operator is being used to increment j as well as i
  for (i = 0; i < pgmlen; j++, i++) {
    prob = rand()/(RAND_MAX+1.);
    if (prob < pmutate) {
      nmod += 1;
      //Have a mutant, now decide which kind:
      mutant_type = (int) (nmut*rand()/(RAND_MAX+1.0));
      switch(mutant_type) {
         case INCREMENT:
            if (this->operator[](i) < INT_MAX/2 -1 ) {
              x[j] = this->operator[](i) + 1;
            }
            else {
              x[j] = 0.;
            }
            break;
         case DECREMENT:
            if (this->operator[](i) > INT_MIN/2 + 1) {
              x[j] = this->operator[](i) - 1;
            }
            else {
              x[j] = 0.;
            }
            break;
         case MULTIPLY:
            tfloat = this->operator[](i);
            tfloat *= 2.*(0.5 - rand()/(RAND_MAX + 1.0)) ;
            if (fabs(tfloat) > INT_MAX/2. - 1.) {
              x[j] = 0.;
            }
            else {
              x[j] = (0.5 + tfloat) ;
            }
            break;
         case DIVIDE:
            tfloat = this->operator[](i);
            tfloat /= 2.*(0.5 - rand()/(RAND_MAX + 1.0)) ;
            if (fabs(tfloat) > INT_MAX/2. - 1.) {
              x[j] = 0;
            }
            else {
              x[j] = (0.5 + tfloat) ;
            }
            break;
         case POINTSWAP:
            if (j+1 < x.length() && i+1 < pgmlen) {
              x[j] = this->operator[](i+1);
              x[j+1] = this->operator[](i);
              j += 1;
              i += 1;
            }
            break;
         case DUPLICATE:
            //CDprintf("Duplication mutation, i,j = %d,%d ndup = %d vs. pgmlen %d\n",i,j, NDUP, pgmlen);
            //CDfflush(stdout);
            if (x.resize(x.length()+NDUP)  == (1==1) ) {
              for (k = 0; k < NDUP; k++) {
                 x[k+j] = 0.;
              }
              for (k = 0; k < NDUP; k++) {
                 x[k+j] = this->operator[]( (i+k) % pgmlen );
              }
              j += NDUP;
            }
            else {
              printf("Resize in DUPLICATE failed\n"); fflush(stdout);
              for (k = 0; k < pgmlen; k++) {
                x[k] = this->operator[](k);
              }
            }
            break;

         default :
            printf("Out of range mutation %d\n",mutant_type);
      } // end of switch
    } // end of mutation clause of if
    else {
       // No mutation, just copy over
       x[j] = this->operator[](i);
    }

    } // end of looping over original program

  return;
}
#endif
