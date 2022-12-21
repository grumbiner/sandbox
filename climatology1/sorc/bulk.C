#include "ncepgrids.h"

void conditional(global_ice<float> &in, global_ice<float> &sum_cond, global_ice<float> &sum2_cond, global_ice<float> &count);
 
int main(int argc, char *argv[]) {
  global_ice<float> in, sum, sum2;
  global_ice<float> sum_cond, sum2_cond;
  global_ice<float> count;
  FILE* fin;
  int i, j;
  palette<unsigned char> gg(19,65);

  sum.set((float) 0.0);
  sum2.set((float) 0.0);
  sum_cond.set((float) 0.0);
  sum2_cond.set((float) 0.0);
  count.set((float) 0);

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    in.binin(fin);
    fclose(fin);

    if (in.gridmax() > 3) in /= 100.;
    for (j = 0; j < in.xpoints()*in.ypoints(); j++) {
      if (in[j] > 1.00 && in[j] <= 1.28) in[j] = 1.00;
      if (in[j] > 1.28) in[j] = 0.0;
    } 


    conditional(in, sum_cond, sum2_cond, count);
    
    sum += in;
    in  *= in;
    sum2 += in;
  }
  sum      /= (argc-1);
  sum2     /= (argc-1);
  for (i = 0; i < in.xpoints()*in.ypoints(); i++) {
    if (count[i] > 0.0) {
      sum_cond[i] /= count[i];
      sum2_cond[i] /= count[i];
    }
  }
  global_ice<float> var, var_cond;

  for (i = 0; i < in.xpoints()*in.ypoints(); i++) {
    var[i] = sum2[i] - sum[i]*sum[i]; 
    var_cond[i] = sum2_cond[i] - sum_cond[i]*sum_cond[i]; 
  }

  fin = fopen("count","w");
  count.binout(fin);
  sum.binout(fin);
  sum2.binout(fin);
  var.binout(fin);
  sum_cond.binout(fin);
  sum2_cond.binout(fin);
  var_cond.binout(fin);
  fclose(fin);

  printf("count     %f %f %f %f\n",count.gridmax(), count.gridmin(), count.average(), count.rms() );
  printf("sum       %5.3f %5.3f %5.3f %5.3f\n",sum.gridmax(), sum.gridmin(), sum.average(), sum.rms() );
  printf("sum2      %f %f %f %f\n",sum2.gridmax(), sum2.gridmin(), sum2.average(), sum2.rms() );
  printf("sum_cond  %5.3f %5.3f %5.3f %5.3f\n",sum_cond.gridmax(), 
                           sum_cond.gridmin(), sum_cond.average(), sum_cond.rms() );
  printf("sum2_cond %f %f %f %f\n",sum2_cond.gridmax(), sum2_cond.gridmin(), 
                           sum2_cond.average(), sum2_cond.rms() );

  printf("var       %f %f %f %f\n",var.gridmax(), var.gridmin(), var.average(), var.rms() );
  printf("var_cond  %f %f %f %f\n",var_cond.gridmax(), 
                           var_cond.gridmin(), var_cond.average(), var_cond.rms() );

  //count.scale();
  //count.xpm("count.xpm",7,gg);
//
  //sum.scale();
  //sum.xpm("sum.xpm",7,gg);
  //sum2.scale();
  //sum2.xpm("sum2.xpm",7,gg);
  //var.scale();
  //var.xpm("var.xpm",7,gg);
//
  //sum_cond.scale();
  //sum_cond.xpm("sum_cond.xpm",7,gg);
  //sum2_cond.scale();
  //sum2_cond.xpm("sum2_cond.xpm",7,gg);
  //var_cond.scale();
  //var_cond.xpm("var_cond.xpm",7,gg);

  return 0;
}
void conditional(global_ice<float> &in,   global_ice<float> &sum, 
                 global_ice<float> &sum2, global_ice<float> &count) {
  int i;

  for (i = 0; i < in.xpoints()*in.ypoints(); i++) {
    //if (in[i] > 1.00 && in[i] < 1.28) in[i] = 1.00;
    if (in[i] > 0 ) {
      sum[i]   += in[i];
      sum2[i]  += in[i]*in[i];
      count[i] += 1;
    }
  }

  return;
}
