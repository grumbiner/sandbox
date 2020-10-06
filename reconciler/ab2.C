#include <stdio.h>

#include "resops.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fouta;
  grid2<float> field(421,403);

  int pad_size;
  float pad_value;
  mvector<float> padding;

  fin = fopen(argv[1],"r");
  fouta = fopen(argv[2],"w");

  pad_size = ((field.xpoints()*field.ypoints() + 4095)/4096)*4096 - field.xpoints()*field.ypoints();
  padding.resize(pad_size);
  pad_value = pow(2.0, 100.0);
  padding = pad_value;

// lon
  field.reader(fin);
  field.binout(fouta);
  padding.binout(fouta);

// lat
  field.reader(fin);
  field.binout(fouta);
  padding.binout(fouta);

  fclose(fouta);

  return 0;
} 
