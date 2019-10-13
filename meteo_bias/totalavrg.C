#include<stdio.h>

int main(char *avrg_sw, char *avrg_lw, char *avrg_temp)
{

FILE *fin_sw, *fin_lw, *fin_temp;
float swdn_avrg, lwdn_avrg, temp_avrg;
int n = 0, m = 0, fscanf(FILE *f, float *swdn_avrg, float *lwdn_avrg, float *temp_avrg);
float swfinalsum = 0, lwfinalsum = 0, tempfinalsum = 0, swfinal, lwfinal, tempfinal;

fin_sw = fopen("avrg_sw", "r");
fin_lw = fopen("avrg_lw", "r");
fin_temp = fopen("avrg_temp", "r");

do {

fscanf(fin_sw, "%f      ", &swdn_avrg);
fscanf(fin_lw, "%f      ", &lwdn_avrg);

swfinalsum = swfinalsum + swdn_avrg;
lwfinalsum = lwfinalsum + lwdn_avrg;

n++;
}
while((!feof(fin_sw))||(!feof(fin_lw)));

do {

fscanf(fin_temp, "%f      ", &temp_avrg);
tempfinalsum = tempfinalsum + temp_avrg;
m++;
}
while(!feof(fin_temp));

swfinal = swfinalsum/n;
lwfinal = lwfinalsum/n;
tempfinal = tempfinalsum/m;

printf("Average for Shortwave is %f.\n", swfinal);
printf("Average for Longwave is %f.\n", lwfinal);
printf("Average for Temperature is %f.\n", tempfinal);

return 0;
}

