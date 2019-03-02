#include <stdio.h>

int main(int argc, char *argv[])
{
  char t19v[465][385];
  char t19h[465][385];
  char t37v[465][385];
  char cout[465][385];
  FILE *f19v, *f19h, *f37v;
  FILE *out1, *out2, *out3;
  int i, j, x1, x2, x3;

  f19v = fopen(argv[1], "r");
  f19h = fopen(argv[2], "r");
  f37v = fopen(argv[3], "r");

  fread(t19v, sizeof(char), 465*385, f19v);
  fread(t19h, sizeof(char), 465*385, f19h);
  fread(t37v, sizeof(char), 465*385, f37v);

  for ( j = 0; j < 385 ; j++)
  {  for ( i = 0; i < 465 ; i++)
     {
     x1 = t19v[i][j]; 
     x2 = t19h[i][j]; 
     x3 = t37v[i][j]; 
     if (x1 >= 128 || x1 < 32) x1 = 0;
     if (x2 >= 128 || x2 < 32) x2 = 0;
     if (x3 >= 128 || x3 < 32) x3 = 0;
     x1 = x1 / 16;
     x2 = x2 / 16;
     x3 = x3 / 16;
     cout[i][j] = x1 + 6*x2 + 36*x3;
     }
  }

  out1 = fopen(argv[4], "w");
  fwrite(cout, sizeof(char), 465*385, out1);

  return 0;


}
