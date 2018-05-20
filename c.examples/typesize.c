#include <stdio.h>

/* Display the sizes of standard types on the current machine */
/* Robert Grumbine 6 June 1994 */

int main() {
  int i;
  long int j;
  short int k;
  float l;
  double m;
  char n;

  printf("%d bytes in an int \n",sizeof(i));
  printf("%d bytes in a long int \n",sizeof(j));
  printf("%d bytes in a short int \n",sizeof(k));
  printf("%d bytes in a float \n",sizeof(l));
  printf("%d bytes in a double \n",sizeof(m));
  printf("%d bytes in a char \n",sizeof(n));

  return 0;

}
