#include "ncepgrids.h"

int main(int argc, char *argv[]) {
    FILE *fin, *fout;
    global_12th<float> orig, post, dist;
    global_12th<float> final, alpha;
    float length = 30; // km
    float dmax;

    fin = fopen(argv[1], "r");
    orig.binin(fin);
    fclose(fin);
    if (orig.gridmax() < 200) orig += 273.15;

    fin = fopen(argv[2], "r");
    post.ftnin(fin);
    fclose(fin);
    if (post.gridmax() < 200) post += 273.15;

    fin = fopen(argv[3], "r");
    dist.binin(fin);
    fclose(fin);

    dmax = dist.gridmax();
    if (dmax > 1e5) dist /= 1000.; // ensure also in km
    if (dist.gridmin() < 0) {
      printf("min distance is negative, oops, %f\n",dist.gridmin() );
      return 1;
    }

    ijpt loc;
    for (loc.j = 0; loc.j < dist.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < dist.xpoints(); loc.i++) {
       alpha[loc] = exp(- dist[loc]*dist[loc] / 2. / length / length); 
    }
    }
    
// New estimate is alpha*post + (1-alpha)*orig
// method is to weight such that points on or very near land are essentially replaced
//   by laplacean filled climatology or climatology, while those away from land remain
//   untouched.
    fout = fopen(argv[4],"w");
    orig.ftnout(fout);
    post.ftnout(fout);

    final = orig;
    orig *= alpha;
    post *= alpha;
    final -= orig;
    final += post;
    final.ftnout(fout);

    fclose(fout);

    return 0;
}
