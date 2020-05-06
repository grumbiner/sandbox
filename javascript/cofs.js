/*
  *************************************************************************
  ***                                                                   ***
  ***   If you can read this, you do not currently have Javascript      ***
  ***   capability.  You will still be able to view all COFS products.  ***
  ***   The extra capability is simply an enhancement, making it easier ***
  ***   to rapidly flip presentation between parameters, depth, and     ***
  ***   nowcast/forecast lead.                                          *** 
  ***                                                                   ***
  *************************************************************************
*/

function select(base, parm, depth, time) {
  image = new Image();
  naming = base+'-'+parm;

  depth *= 1.;
  //alert("depth = "+depth);
  if (depth == 0) {
    naming = naming + "sig01";
  }
  else if (depth >= 1. && depth <= 5000.) {
    if (depth >= 1000.) {
      naming = naming + depth + "m";   
    }
    else if (depth >= 100.) {
      naming = naming + "0" + depth +"m";
    }
    else if (depth >= 10.) {
      naming = naming + "00" + depth +"m";
    }
    else {
      naming = naming + "000" + depth +"m";
    }
  }
  else {
     naming = naming + "sig18-";
  }

  if (time == "ncst") {
    naming = naming + "-ncst.gif";
  }
  else if (time == "fcst") {
    naming = naming + "-fcst.gif";
  }

  alert(naming);
  image.src = naming;
  document.cofsframe.src = image.src;
}
