#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_ice<float> in, sum, sum2;
  global_ice<float> sum_cond, sum2_cond;
  global_ice<float> var, var_cond;
  global_ice<float> count;
  FILE* fin;
  int i, total = 0;
  ijpt loc;
  latpt ll;
  palette<unsigned char> gg(19,65);

  fin = fopen("count","r");
  count.binin(fin);
  sum.binin(fin);
  sum2.binin(fin);
  var.binin(fin);
  sum_cond.binin(fin);
  sum2_cond.binin(fin);
  var_cond.binin(fin);
  fclose(fin);

  for (i = 0; i < in.xpoints()*in.ypoints(); i++) {
    if (count[i] > 0) { 
      total += 1;
    loc.i = i % in.xpoints();
    loc.j = i / in.xpoints();
    ll = in.locate(loc);
      printf("%d  %3d %3d  %6.2f %7.2f  %5d  %6.4f %7.5f %7.5f  %6.4f %7.5f %7.5f\n",i,loc.i, loc.j, ll.lat, ll.lon,
      (int) count[i], sum[i], sqrt(sum2[i]), sqrt(var[i]),  sum_cond[i], sqrt(sum2_cond[i]) , sqrt(var_cond[i]) ); 
    }
  }
  printf("number of points that ever see ice: %d\n",total);

  count.scale();
  sum.scale();
  sum2.scale();
  var.scale();
  sum_cond.scale();
  sum2_cond.scale();
  var_cond.scale();

  count.xpm("count.xpm",7,gg);
  sum.xpm("sum.xpm",7,gg);
  sum2.xpm("sum2.xpm",7,gg);
  var.xpm("var.xpm",7,gg);
  sum_cond.xpm("sum_cond.xpm",7,gg);
  sum2_cond.xpm("sum2_cond.xpm",7,gg);
  var_cond.xpm("var_cond.xpm",7,gg);

  return 0;
}
