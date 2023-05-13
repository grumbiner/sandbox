#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "ncepgrids.h"
#include "nesdis.h"
#include "surface_tags.h"

void filter(metricgrid<unsigned char> &gfilt, metricgrid<int> &gland, metricgrid<int> &gsea, metricgrid<float> &gpct, metricgrid<unsigned char> &oldland, char *outname);

template <class T>
void showregion(char *label, latpt ll, latpt ur, metricgrid<T> &gfilt,
                  metricgrid<T> &oldland);

int main(int argc, char *argv[])
{
  dirrec header;
  block  tags;
  short int unblocked[160][160], tmp;
  llgrid<int> *gland, *gsea;
  llgrid<unsigned char> *gfilt, *oldland;
  llgrid<float> *gpct;

  FILE *fin, *fout;
  ijpt x;
  fijpt y;
  latpt ll, ur;
  palette<unsigned char> gg(19,65);

  int i, k, l;
  float latb, lonb, dll;
  int nx, ny;

  dll = atof(argv[1]);
  nx = (int) (360./dll + 0.5);
  ny = (int) (180./dll + 0.5);

  gland = new llgrid<int>(nx, ny, -dll, dll, 90. - dll/2., dll/2.);
  gsea  = new llgrid<int>(nx, ny, -dll, dll, 90. - dll/2., dll/2.);
 
/* Initialize the ssmi grid counters */
  tmp = 0;
  gland->set(tmp);
  gsea->set(tmp);
  
/* Now get hold of the nesdis data */
  fin = fopen("temphigh","r");
  fread(&header, sizeof(dirrec), 1, fin);

  for (i = 0; i < 648; i++) { 
/* Decode a tag block */
    if (header.recno[i] == RECSEA) {
      blocksea(&unblocked[0][0]);    
    }
    else if (header.recno[i] == RECLAND) {
      blockland(&unblocked[0][0]);    
    }
    else {
      fseek(fin, (header.recno[i]-1)*3200, SEEK_SET);
      fread(&tags, sizeof(block), 1, fin);
      unblock(&unblocked[0][0], &tags);
    }

/* Remap over to the destination grid */
    latb = -90.0 + 10.0*( (int) (i/36) );
    lonb = -180. + 10.0*( i % 36 );

    for (k = 0; k < 160; k++) {
      ll.lat = latb + ((float)k)/PER_DEGREE ;

      for (l = 0; l < 160; l++) {
        ll.lon = lonb + ((float)l)/PER_DEGREE ;

        y = gland->locate(ll);
        x.i = (int)(y.i+ 0.5);
        x.j = (int)(y.j+ 0.5);

          if (x.i >= 0 && x.i < nx && x.j >= 0 && x.j < ny) {
            if (unblocked[k][l] == LAND) {
              gland->operator[](x) += 1;
            }
            else if (unblocked[k][l] == SEA) {
              gsea->operator[](x) += 1;
            }
            else {
              printf("unblock value out of range %d %d %d\n",k,l,unblocked[k][l]);
              return -1;
            }
          } /* end of filling in tags */


      } /* end of looping across longitude */
    } /* end of looping across latitude within block */
  } /* end of looping across blocks */

  fout=fopen("globe","w");
  printf("land max min %d %d\n",gland->gridmax(), gland->gridmin() );
  printf("sea  max min %d %d\n",gsea ->gridmax(), gsea ->gridmin() );
  gland->xpm("landno.xpm",7,gg);
  gsea->xpm("seano.xpm",7,gg);
  gland->binout(fout);
  gsea->binout(fout);
  fclose(fout);
  fclose(fin);

//////////////////////////////////////////////////////////////////
// Now incorporate the filtering -- This should be a function:
  gpct = new llgrid<float>(nx, ny, -dll, dll, 90. - dll/2., dll/2.);
  gfilt = new llgrid<unsigned char>(nx, ny, -dll, dll, 90. - dll/2., dll/2.);
  oldland = new llgrid<unsigned char>(nx, ny, -dll, dll, 90. - dll/2., dll/2.);
  fin = fopen(argv[2], "r");
  oldland->binin(fin);
  fclose(fin);
  gfilt->set(100);
  gpct->set(1.0);
  printf("Integral %f\n",gpct->integrate() / 1.e12);
  gpct->set(125.0);

  filter(*gfilt, *gland, *gsea, *gpct, *oldland, argv[3]);

//////////////////////////////////////////////////////////////////////////
////////////////// Print out grids for a couple selected areas which should have
//////// This too should be a function
  ll.lat = 59.0;
  ll.lon = -171.0;
  ur.lat = 56.0;
  ur.lon = -168.0;
  showregion("Pribilof islands\n", ll, ur, *gfilt, *oldland);

  ll.lat = 50.0;
  ll.lon = -93.0;
  ur.lat = 41.0;
  ur.lon = -80.0;
  showregion("Great Lakes\n", ll, ur, *gfilt, *oldland);

 
  return 0;

}
#include "filter.C"
#include "region.C"
