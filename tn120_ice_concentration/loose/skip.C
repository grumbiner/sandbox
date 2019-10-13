#include "ncepgrids.h"
// 25 June 2014
// Construct file of points to skip in sea ice model skill assessment
//   based on the a posteriori analysis filter file.
// Robert Grumbine

int main(int argc, char *argv[]) {
  FILE *fland, *fpost, *fskip;
  global_12th<unsigned char> land, post, skip;
  ijpt loc;

  fland = fopen(argv[1], "r");
  fpost = fopen(argv[2], "r");
  fskip = fopen(argv[3], "w");

  land.binin(fland); fclose(fland);
  post.binin(fpost); fclose(fpost);

  for (loc.j = 0 ; loc.j < skip.ypoints(); loc.j++) {
  for (loc.i = 0 ; loc.i < skip.xpoints(); loc.i++) {
    if ((land[loc] > 0 ) || 
        (post[loc] == 158 ) ||
        (post[loc] == 159 ) ||
        (post[loc] == 160 ) ||
        (post[loc] == 161 ) ||
        (post[loc] == 162 ) ||
        (post[loc] == 163 ) ||
        (post[loc] == 164 ) ||
        (post[loc] == 170 ) ||
        (post[loc] == 171 ) ||
        (post[loc] == 172 ) 
       ) {
      skip[loc] = 1;
    }
    else {
      skip[loc] = 0;
    }
  }
  }

  skip.binout(fskip);
  fclose(fskip);

  return 0;
}
