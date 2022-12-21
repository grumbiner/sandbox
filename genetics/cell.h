#ifndef CELL_H
#define CELL_H

class cell {
  public:
    int a[3][3];
    cell();
    cell(cell &);
    void set(int, int, int);
    void zero();
    void add(cell &);
    void mul(cell &);
    void operator=(cell &);
    void show();
    void binin(FILE *);
    friend double score(cell &, cell&);
    friend double score2(cell &, cell&);
    friend double score3(cell &, cell&);
};
void cell::show() {
   int j;
   for (j = 0; j < 3; j++) {
      printf("%d %d %d\n",a[0][j], a[1][j], a[2][j]);
   }
   fflush(stdout);
}
void cell::binin(FILE *fin) {
// Note that we're rescaling the data.
  unsigned char x[3][3];
  int i, j;
  fread(x, sizeof(unsigned char), 9, fin);
  for (j = 0; j < 3; j++) {
  for (i = 0; i < 3; i++) {
    a[i][j] = ((int) x[i][j]) ;
  }
  }
  return;
}
cell::cell() {
  int i, j;
  for (j = 0; j < 3; j++) {
  for (i = 0; i < 3; i++) {
     a[i][j] = 0;
  }
  }
}
void cell::zero() {
  int i, j;
  for (j = 0; j < 3; j++) {
  for (i = 0; i < 3; i++) {
     a[i][j] = 0;
  }
  }
  return;
}
cell::cell(cell &x) {
  int i, j;
  for (j = 0; j < 3; j++) {
  for (i = 0; i < 3; i++) {
     a[i][j] = x.a[i][j];
  }
  }
}
/////////// These are the operations that can be scheduled /////////////////
void cell::set(int i, int j, int val) {
// interior sanity checking

  if (i < 3 && i >= 0 && j < 3 && j >= 0) {
    a[i][j] = val;
  }
  else {
    //printf("out of range point\n"); fflush(stdout);
  }
}

void cell::add(cell &x) {
  int i, j;

  for (j = 0; j < 3; j++) {
  for (i = 0; i < 3; i++) {
     //printf("add %d %d %d %d\n",i,j, a[i][j], x.a[i][j]); fflush(stdout);
     a[i][j] += x.a[i][j];
  }
  }
  return ;
}
void cell::operator=(cell &x) {
  int i, j;

  for (j = 0; j < 3; j++) {
  for (i = 0; i < 3; i++) {
     a[i][j] = x.a[i][j];
  }
  }

  return ;
}
void cell::mul(cell &x) {
  int i, j, k;
  float tsum;
  cell tmp;

  for (j = 0; j < 3; j++) {
    for (i = 0; i < 3; i++) {
       if (x.a[i][j] < -INT_MAX/2) {
           printf("Resetting x.a in mul\n"); fflush(stdout);
           x.a[i][j] = 0;
       }
       if (x.a[i][j] > INT_MAX/2) {
           printf("Resetting x.a in mul\n"); fflush(stdout);
           x.a[i][j] = 0;
       }
       if (a[i][j] < -INT_MAX/2) {
           printf("Resetting a in mul\n"); fflush(stdout);
           a[i][j] = 0;
       }
       if (a[i][j] > INT_MAX/2) {
           printf("Resetting a in mul\n"); fflush(stdout);
           a[i][j] = 0;
       }
    }
  }

  for (j = 0; j < 3; j++) {
    for (i = 0; i < 3; i++) {
      tsum = 0.0;
      for (k = 0; k < 3; k++) {
        tsum += a[i][k] * x.a[k][j]  /100.;
      }
      if (fabs(tsum) < INT_MAX/2. - 1 ) {
        //printf("No overflow in mul\n"); fflush(stdout);
        tmp.a[i][j] = (int) (0.5 + tsum);
      }
      else {
        //printf("Prevented overflow in mul\n"); fflush(stdout);
        tmp.a[i][j] = 0;
      }
    }
  }
  for (j = 0; j < 3; j++) {
    for (i = 0; i < 3; i++) {
       this->a[i][j] = tmp.a[i][j];
    }
  }
  return ;
}
///////////////////////////////////
double score(cell &a2, cell &a1) {
  double tmp;

  tmp= (a2.a[1][1] - a1.a[1][1])*(a2.a[1][1] - a1.a[1][1]) ;

  return tmp;
}
double score2(cell &a2, cell &a1) {
  double tmp;

  tmp = (a2.a[1][1] - a1.a[1][1])*(a2.a[1][1] - a1.a[1][1]) ;
  tmp += (a2.a[2][2] - a1.a[2][2])*(a2.a[2][2] - a1.a[2][2]) ;

  return tmp;
}
double score3(cell &a2, cell &a1) {
  double tmp;

  tmp = (a2.a[1][1] - a1.a[1][1])*(a2.a[1][1] - a1.a[1][1]) ;
  tmp += (a2.a[2][2] - a1.a[2][2])*(a2.a[2][2] - a1.a[2][2]) ;
  tmp += (a2.a[0][0] - a1.a[0][0])*(a2.a[0][0] - a1.a[0][0]) ;

  return tmp;
}

#endif
