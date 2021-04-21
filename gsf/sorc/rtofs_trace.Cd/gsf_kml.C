// Write out a line (gulf stream or loop current) in kml format
// derived from sea ice edge finder
// Future: Add to mvector?  Friend to mvector?

#include "mvector.h"
#include "metric.h" // include for definition of arcdis

void gsf_kml(mvector<latpt> &x, FILE *fout, latpt view, char* name) ;

void gsf_kml(mvector<latpt> &x, FILE *fout, latpt view, char* name) {
  float eye_alt = 3000.*1000.; // 3000 km
  float dist, lat, lon, lat2, lon2;
  int i;
  float mindist = 2.;   // km
  float maxdist = 200.;

  if (view.lon > 180 ) view.lon -= 360.;
  if (view.lon < -180) view.lon += 360.;

// Header information:
  fprintf(fout, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  fprintf(fout, "<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n");
  fprintf(fout, "<Document>\n");

  fprintf(fout, "<Folder>\n");
  fprintf(fout, "  <name>Not For Navigation Purposes!</name>\n");
  fprintf(fout, "  <LookAt>\n");
  fprintf(fout, "   <longitude>%f</longitude>\n",view.lon);
  fprintf(fout, "    <latitude>%f</latitude>\n",view.lat);
  fprintf(fout, "    <range>%f</range>\n",eye_alt);
  fprintf(fout, "  </LookAt>\n");

  fprintf(fout, " <Placemark>\n");
  fprintf(fout, " <name>%s</name>\n",name);
  fprintf(fout, " <LineString>\n");
  fprintf(fout, "     <tessellate>1</tessellate>\n");
  fprintf(fout, "     <coordinates>\n");

  i = 0;
  lat = x[i].lat;
  lon = x[i].lon;
  if (lon > 180) {
    lon -= 360.;
  }
  fprintf(fout,"%f,%f,0\n",lon, lat);

  for (i = 1; i < x.xpoints(); i++) {
    lat2 = x[i].lat;
    lon2 = x[i].lon;
    if (lon2 > 180) {
      lon2 -= 360.;
    }
    #ifdef IBM
    dist = arcdis(lon, lat, lon2, lat2);
    #else
    dist = arcdis_(lon, lat, lon2, lat2);
    #endif
    if (dist > mindist && lat > 0 && dist < maxdist) {
      fprintf(fout,"%f,%f,0\n",lon2, lat2);
      lat = lat2; 
      lon = lon2;
    }
  }

  // close up all tags:
  fprintf(fout, "</coordinates>\n");
  fprintf(fout, "</LineString>\n");
  fprintf(fout, "</Placemark>\n");

  fprintf(fout, "</Folder>\n");
  fprintf(fout, "</Document>\n");
  fprintf(fout, "</kml>\n");


  return;
}
