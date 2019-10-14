#include <stdio.h>
#include <math.h>

#define NPRIMES 39
//#define NPRIMES 10

int main(int argc, char *argv[]) {
   int primes[NPRIMES] = { 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 61, 67, 73, 89, 97, 101, 103, 109, 113, 127, 137, 139, 151, 211, 223, 241, 251, 281, 353, 373, 439, 449, 521, 617, 641, 643, 859 };
  long long int x, rem, y, yrem;
  int caught, base = 10, final = 3;
  int i,j,k;

  printf("sizes %d %d\n",sizeof(long int), sizeof(long long int) );

  for (k = 4; k < 80; k += 1)  {
    caught = 0;
    for (i = 0; i < NPRIMES; i++) {
      rem = 0;
      x = 1;
      y = 1;
      for (j = 1; j < k; j++) {
        x *= base;
        y *= base;
        if (rem > x) {
          printf("prime = %d  x = %lld rem = %lld\n",primes[i], x, rem);
          fflush(stdout);
          return 1;
        }
        //printf("i,j,k %d %d %d  prime,x,rem %lld %lld %lld\n",i,j,k,primes[i], x,rem);

        if (x >= primes[i]) {
          rem = x % primes[i];
          x = rem;
        }
        else {
          rem = rem * base;
          x   = x;
        }
        
      }
      rem *= base; rem += final;
      y   *= base;   y += final;
      yrem = y % primes[i];
      //printf ("%d trying %d on %lld rem = %lld %lld yrem = %lld\n",i, primes[i], y, rem, rem%primes[i], yrem);

      if ((rem % primes[i]) == 0 ) {
        caught = 1;  
        if (k <= 18) {printf("not %2d because of %3d  %lld\n",k,primes[i], y); }
          else {printf("not %2d because of %3d\n",k,primes[i]);} 
        break;
      }

    }      
    if (caught == 0) {
      if (k <= 18) {printf("k = %4d is a candidate %lld\n",k, y); }
        else { printf("k = %4d is a candidate\n",k); }
    }
  }

  return 0;
}
/*
1289
1409
2161
2531
2689
3169
3541
4093
5051
7841
8779
9091
9901
19841
27961
52579
60101
69857
98641
153469
206209
226549
459691
909091
976193
1580801
1676321
4188901
5070721
5882353
6187457
7019801
28559389
39526741
70541929
99990001
121499449
599144041
1052788969
1056689261
1491383821
5964848081
14175966169
66554101249
*/
