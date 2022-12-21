#include "ncepgrids.h"

int main(void) {
   global_12th<unsigned char> x, l;
   mvector<int> values(256);
   FILE *fin;
   int i;
   ijpt loc;

   fin = fopen("fout_i","r");
   x.binin(fin);
   fclose(fin);

   values = 0;
   for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
   for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      values[(int) x[loc] ]  += 1;
   }
   }
  
   int tot = 0; 
   for (i = 0; i < values.xpoints(); i++) {
     if (values[i] != 0) {
       printf("%3d %7d\n",i, values[i]);
       tot += values[i];
     }
   }
   printf("total = %d\n",tot);

   x.laplace(l);
   values = 0;
   for (loc.j = 0; loc.j < l.ypoints(); loc.j++) {
   for (loc.i = 0; loc.i < l.xpoints(); loc.i++) {
      values[(int) l[loc] ]  += 1;
   }
   }

   tot = 0; 
   for (i = 0; i < values.xpoints(); i++) {
     if (values[i] != 0) {
       printf("%3d %7d\n",i, values[i]);
       tot += values[i];
     }
   }
   printf("total = %d\n",tot);

    

   return 0;
}
