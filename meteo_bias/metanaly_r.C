#include "ncepgrids.h"

// Analyze metfile contents 

const double sigma = 5.67E-8; //Wm^-2K^-4

void checker(metricgrid<float> &h, char *field, char *param, ijpt &sp) ;

int main(int argc, char *argv[], char *range, char *avrg_lw, char *avrg_temp) {
  northgrid<float> tair, slp, rh, tsfc, swdn, lwdn, mask;
  char field[900], parameter[900];

  FILE *fin, *fout_sw, *fout_lw, *fout_temp, *fout_range;
  int i, n = 0, fprintf(FILE *f1, float *swdn_avrg, float *lwdn_avrg, float *temp_avrg);
  ijpt sploc, tloc;
  
  latpt ll;
  ll.lon = 0;
  ll.lat = 90;
  sploc = swdn.locate(ll);
  float LW, lwtemp, swdn_sum = 0, lwdn_sum = 0, swdn_avrg, lwdn_avrg;
  float temp_avrg, temp_sum = 0;                     //lwtemp in Kelvin

  fin = fopen (argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open input file %s\n", argv[1]);
    return 1;
  }
  tair.ftnin(fin);

  slp.ftnin(fin);

  rh.ftnin(fin);

  tsfc.ftnin(fin);

  swdn.ftnin(fin);
  sprintf(field, "Downwelling Shortwave");
  sprintf(parameter, "swdn");
//  checker(swdn, field, parameter, sploc);

  lwdn.ftnin(fin);
  sprintf(field, "Downwelling Longwave");
  sprintf(parameter, "lwdn");
//  checker(lwdn, field, parameter, sploc);

  fout_range=fopen("range","a");

  for (tloc.j=0; tloc.j < lwdn.ypoints(); tloc.j++) {
       for(tloc.i=0; tloc.i < lwdn.xpoints(); tloc.i++) {
		
        ll= tair.locate(tloc);

	if(ll.lat >= 40){
	swdn_sum = swdn_sum + swdn[tloc];
        
        lwtemp = tair[tloc] + 273.15;
	LW = sigma * pow(lwtemp,4);

	lwdn_sum = lwdn_sum + LW;
        
        temp_sum = temp_sum + lwtemp;

//        printf("Temp = %f; ", lwtemp);

        if(swdn[tloc] > 1000){ 
         	fprintf(fout_range, "*DnSw out of range* ");
		fprintf(fout_range, "DnSw=%f; ", swdn[tloc]);
		fprintf(fout_range, "Lat%f Lon%f;\n", ll.lat, ll.lon);
		}
 	 
//	printf("%f ", tair[tloc]);

	if(LW <= 100){
		fprintf(fout_range, "*DnLw under range* ");
		fprintf(fout_range, "DnLw=%f; ", LW);
        	fprintf(fout_range, "Lat%f Lon%f;\n", ll.lat, ll.lon);
		}
  	else if (LW >= 475){
		fprintf(fout_range, "*DnLw over range* ");
		fprintf(fout_range, "DnLw=%f; ", LW);
		fprintf(fout_range, "Lat %f Lon %f;\n", ll.lat, ll.lon);
		}
        n++;
	}

       } 
  } 
  mask.ftnin(fin);
   

//  fout_lw=fopen("avrg_lw","a");
//  fout_temp=fopen("avrg_temp","a");

//  if ((fout_sw != (FILE *) NULL)&&(fout_lw != (FILE *) NULL)&&(fout_temp != (FILE *) NULL))
//  printf("Succesfull opening of the average files in w mode.\n");
//  else
//  printf("Unsuccesfull to open one of the output files.\n");

//  swdn_avrg = swdn_sum/n;
//  lwdn_avrg = lwdn_sum/n;
//  temp_avrg = temp_sum/n;  

//temp_avrg = pow((LW/sigma),.25)

//  fprintf(fout_sw, "%f      ", swdn_avrg);
//  fprintf(fout_lw, "%f      ", lwdn_avrg);
//  fprintf(fout_temp, "%f      ", temp_avrg);

  return 0;
} 

void checker(metricgrid<float> &h, char *field, char *param, ijpt &sp) {
  ijpt loc;
  latpt ll;
 
  printf("%s\n",field);
  printf("max, min, average, rms %f %f %f %f  %f\n",h.gridmax(), h.gridmin(),             h.average(0.), h.rms(0.), h[sp]);

  for (loc.j = 0; loc.j < h.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < h.xpoints(); loc.i++) {
     if (! finite(h[loc]) ) {
       ll = h.locate(loc);
       printf("Found nan in %s at %d %d %f %f  %f\n",param, loc.i, loc.j, 
             ll.lat, ll.lon, h[loc]);
     }
  }
  }

} 
