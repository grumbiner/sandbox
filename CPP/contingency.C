// compute the contingency table matchups
// if a11 etc are double, use weighting by cell area
//   else, merely match point for point.
// Note that in the indices, first one refers to the model, second to observations
//   1 means 'has ice', 2 means 'does not have ice' ( > than cutoff level)
//   so a12 means model has ice, but observation does not (false alarm)
//      a21 means model does not have ice, but observation does (false confidence)

#include "llgrid.h"

void contingency(llgrid<float> &obs, llgrid<float> &model,
              double &a11, double &a12, double &a21, double &a22) ;

void contingency(llgrid<float> &obs, llgrid<float> &model, grid2<unsigned char> &skip, float &level,
              double &a11, double &a12, double &a21, double &a22) ;

template <class T>
void contingency_derived(T a11, T a12, T a21, T a22, float &pod, float &far, float &fcr, float &pct, float &ts, float &bias);

// compute the contingency table matchups
// // if a11 etc are double, use weighting by cell area
// //   else, merely match point for point.
// // Note that in the indices, first one refers to the model, second to observations
// //   1 means 'has ice', 2 means 'does not have ice' ( > than cutoff level)
// //   so a12 means model has ice, but observation does not (false alarm)
// //      a21 means model does not have ice, but observation does (false confidence)
//
template <class T>
void contingency_derived(T a11, T a12, T a21, T a22, float &pod, float &far, 
                            float &fcr, float &pct, float &ts, float &bias) {
   pod = (double) a11 / (double) (a11 + a12);
   far = (double) a12 / (double) (a12 + a11);
   fcr = (double) a21 / (double) (a21 + a22);
   pct = ((double) a11 + (double) a22) / ( (double) (a11+a12+a21+a22)); //percent correct
   ts  = (double) a11 / ( (double) (a11+a12+a21) ); // threat score, aka csi - critical success index
   bias = (double) (a11+a12) / ((double)(a11+a21));

  return;
}

 
// use same code for both with and without -- just that if there's no 
//     skip mask, create one that does nothing
void contingency(llgrid<float> &obs, llgrid<float> &model,
              double &a11, double &a12, double &a21, double &a22) {
  grid2<unsigned char> skip(obs.xpoints(), obs.ypoints());
  float level = 0;
  skip.set(0);
  contingency(obs, model, skip, level, a11, a12, a21, a22);
  return;
}

// Scoring for contingency table:

void contingency(llgrid<float> &obs, llgrid<float> &model, grid2<unsigned char> &skip, float &level,
              double &a11, double &a12, double &a21, double &a22) {
  double area = 0;
  ijpt loc;

  a11 = 0;
  a12 = 0;
  a21 = 0;
  a22 = 0;
  for (loc.j = 0; loc.j < obs.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < obs.xpoints(); loc.i++) {
    // only score if point is water of interest
    if ( skip[loc] == 0 ) {
    
    area += obs.cellarea(loc);
    
    // contingency table:
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += obs.cellarea(loc);
       }
       else {
         a12 += obs.cellarea(loc);
       }
     }
     else {
       if (obs[loc] > level ) {
         a21 += obs.cellarea(loc);
       }
       else {
         a22 += obs.cellarea(loc);
       }
     }
    }

  }
  }

  a11 /= area;
  a12 /= area;
  a21 /= area;
  a22 /= area;

  return;
}
