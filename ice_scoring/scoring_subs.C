#include "ncepgrids.h"
//Robert Grumbine

// Declare functions
void preskip(llgrid<float> &conc, llgrid<unsigned char> &skip) ;

// do the verification on the analysis grid:
void scoring(global_12th<float> &obs, global_12th<float> &model,
              float &mean, float &rms,
              double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) ;
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<float> &count,
              float &mean, float &rms,
              double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) ;

// note that when skipping, 1 = do skip. 
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip, global_12th<float> &count,
              float &mean, float &rms,
              double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) ;
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip,
              float &mean, float &rms,
              double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) ;

// Begin code:

void scoring(global_12th<float> &obs, global_12th<float> &model, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) {
  global_12th<float> delta;
  ijpt loc;
  double globe = 0.0, level = 0.0;

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;

  delta = obs;
  delta -= model;
  mean = 0.0;
  rms  = 0.0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    globe += delta.cellarea(loc);
    mean += delta[loc]*delta.cellarea(loc);
    rms  += delta[loc]*delta[loc]*delta.cellarea(loc);
  }
  }
  mean /= globe;
  rms   = sqrt(rms/globe); 

  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += model.cellarea(loc);
      }
      else {
        a12 += model.cellarea(loc);
      }
    }
    else {
      if (obs[loc] > level ) {
        a21 += model.cellarea(loc);
      }
      else {
        a22 += model.cellarea(loc);
      }
    }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  pod = a11 / (a11 + a21 + a12);
  far = a12 / (a11 + a21 + a12);
  correct = (a11 + a22) / (a11 + a12 + a21 + a22);

  return;
}
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip, global_12th<float> &count, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) {
  global_12th<float> delta;
  ijpt loc;
  double globe = 0.0, level = 0.0;

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;

  delta = obs;
  delta -= model;
  mean = 0.0;
  rms  = 0.0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (skip[loc] == 0) {
      if (count[loc] < 2) {
        globe += delta.cellarea(loc);
        mean += delta[loc]*delta.cellarea(loc);
        rms  += delta[loc]*delta[loc]*delta.cellarea(loc);
      }
      else {
        globe += delta.cellarea(loc)/count[loc];
        mean += delta[loc]*delta.cellarea(loc)/count[loc];
        rms  += delta[loc]*delta[loc]*delta.cellarea(loc)/count[loc];
      }
    }
  }
  }
  mean /= globe;
  rms   = sqrt(rms/globe); 

  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
  if (skip[loc] == 0) {
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += model.cellarea(loc);
      }
      else {
        a12 += model.cellarea(loc);
      }
    }
    else {
      if (obs[loc] > level ) {
        a21 += model.cellarea(loc);
      }
      else {
        a22 += model.cellarea(loc);
      }
    }
  }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  pod = a11 / (a11 + a21);
  far = a12 / (a11 + a12);
  correct = (a11 + a22) / (a11 + a12 + a21 + a22);

  return;
}
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) {
  global_12th<float> delta;
  ijpt loc;
  double globe = 0.0, level = 0.0;

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;

  delta = obs;
  delta -= model;
  mean = 0.0;
  rms  = 0.0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (skip[loc] == 0) {
      globe += delta.cellarea(loc);
      mean += delta[loc]*delta.cellarea(loc);
      rms  += delta[loc]*delta[loc]*delta.cellarea(loc);
    }
  }
  }
  //printf("after skip = %f  ",globe / 1e12);
  mean /= globe;
  rms   = sqrt(rms/globe); 

  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
  if (skip[loc] == 0) {
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += model.cellarea(loc);
      }
      else {
        a12 += model.cellarea(loc);
      }
    }
    else {
      if (obs[loc] > level ) {
        a21 += model.cellarea(loc);
      }
      else {
        a22 += model.cellarea(loc);
      }
    }
  }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  pod = a11 / (a11 + a21);
  far = a12 / (a11 + a12);
  correct = (a11 + a22) / (a11 + a12 + a21 + a22);

  return;
}
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<float> &count, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) {
  global_12th<float> delta;
  ijpt loc;
  double globe = 0.0, level = 0.0;

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;

  delta = obs;
  delta -= model;
  mean = 0.0;
  rms  = 0.0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    globe += delta.cellarea(loc)/count[loc];
    mean += delta[loc]*delta.cellarea(loc)/count[loc];
    rms  += delta[loc]*delta[loc]*delta.cellarea(loc)/count[loc];
  }
  }
  mean /= globe;
  rms   = sqrt(rms/globe); 

  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += model.cellarea(loc)/count[loc];
      }
      else {
        a12 += model.cellarea(loc)/count[loc];
      }
    }
    else {
      if (obs[loc] > level ) {
        a21 += model.cellarea(loc)/count[loc];
      }
      else {
        a22 += model.cellarea(loc)/count[loc];
      }
    }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  pod = a11 / (a11 + a21 + a12);
  far = a12 / (a11 + a21 + a12);
  correct = (a11 + a22) / (a11 + a12 + a21 + a22);

  return;
}
void preskip(llgrid<float> &conc, llgrid<unsigned char> &skip) {
// Zero out all points that will eventually be skipped in scoring comparisons 
//  -- important in that the observation grid tries to analyze all points that
//  are coastal, but coast is skipped in scoring.
  int loc;

  for (loc = 0; loc < conc.xpoints()*conc.ypoints(); loc++) {
    if (skip[loc] == 1) {
      conc[loc] = 0;
    }
  }

  return;
}
void findedge(global_12th<float> obsd, global_12th<float> model, global_12th<float> &edge, global_12th<unsigned char> &near_edge);

void alaska(metricgrid<unsigned char> &x) ;
void northern(metricgrid<unsigned char> &x) ;
void southern(metricgrid<unsigned char> &x) ;

// note that this is not &arg for the input obsd and model concentrations -- we don't want to modify the 
// originals.
void findedge(global_12th<float> obsd, global_12th<float> model, global_12th<float> &edge, global_12th<unsigned char> &near_edge) {
  ijpt loc;
  float flag = 1;
  global_12th<unsigned char> counto, countm;
  int range = 1;

  // Find the points which are near the ice edge.  'near' tbd, but say, 500 km
  // Also, do so for all points near either the observed or modelled edge
  // Create uchar flags for later use
  for (loc.j = 0; loc.j < obsd.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < obsd.xpoints(); loc.i++) {
    if (obsd[loc] > 0) { obsd[loc] = 1;}
    else { obsd[loc] = 0;}

    if (model[loc] > 0) { model[loc] = 1;}
    else { model[loc] = 0;}
  }
  }
  for (loc.j = range; loc.j < obsd.ypoints()-range; loc.j++) {
  for (loc.i = range; loc.i < obsd.xpoints()-range; loc.i++) {
     counto[loc] = obsd.anyof(flag, range, loc);
     countm[loc] = model.anyof(flag, range, loc);
  }
  }

  int all = (2*range+1)*(2*range+1);
  edge.set((unsigned char) 0);
// note issues for the seam along 0 E, and at both poles
  for (loc.j = range; loc.j < obsd.ypoints()-range; loc.j++) {
  for (loc.i = range; loc.i < obsd.xpoints()-range; loc.i++) {
    if (counto[loc] != 0 && counto[loc] != all) {
      edge[loc] = 1;
    } 
    if (countm[loc] != 0 && countm[loc] != all) {
      edge[loc] = 1;
    }
  }
  }

  // now flag points out to 'near' km, sweeping through latitude only
  ijpt tloc;
  float delta = 111.2*1/12.;
  int near = (int) (0.5 + 250. / delta);
  conv(edge, near_edge);

  for (loc.i = 0; loc.i < obsd.xpoints() - range; loc.i++) {
    tloc.i = loc.i;
    for (loc.j = range; loc.j < obsd.ypoints() - range; loc.j++) {
      if (near_edge[loc] == 1) {
        for (tloc.j = max(0, loc.j - near); tloc.j < min( obsd.ypoints(), loc.j+near); tloc.j++) {
          if (near_edge[tloc] == 0) {near_edge[tloc] = 2;}
        }
      }
    }
  }

  // have to flip for skip approach, 1 = do skip
  for (loc.j = 0; loc.j < obsd.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < obsd.xpoints() ; loc.i++) {
    if (near_edge[loc] == 0) {near_edge[loc] = 1;}
    else {near_edge[loc] = 0;}
  }
  }


  return;
}
void northern(metricgrid<unsigned char> &x) {
  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    if (ll.lat < 0) x[loc] = 1;
  }
  }
  return;
}  
void southern(metricgrid<unsigned char> &x) {
  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    if (ll.lat > 0) x[loc] = 1;
  }
  }
  return;
}  

void alaska(metricgrid<unsigned char> &x) {
  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    if (ll.lon < 0) ll.lon += 360.;
    if (ll.lat < 40 || ll.lon > 240 || ll.lon < 150) {
      x[loc] = 1;
    }
  }
  }
  return;
}
